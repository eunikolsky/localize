{-# LANGUAGE QuasiQuotes #-}

module DaemonSpec where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import System.Directory

import Daemon

testDirectory :: FilePath
testDirectory = "test/daemonspec"

daemonTests :: TestTree
daemonTests = testGroup "startDaemon"
  [ testCase "creates output directories" $ do
      cleanDirectory testDirectory
      withCurrentDirectory testDirectory $ do
        mapM_ createDirectory ["dir", "lang", "lang/en"]
        let configText = [r|
          {"watchDirs": {
            "dir": "out",
            "lang": "fake",
            "lang/en": "fake/en"
          }}
        |]
        config <- readConfig configText
        thread <- forkIO $ startDaemon localizeJSON config

        threadDelay 10000 -- TODO is 10 ms enough? refactor to a waiting call
        assertDirectoriesExist ["out", "fake", "fake/en"]

        killThread thread

  , after AllFinish "creates" $
    -- ^ this creates a dependency on the first test so that they don't run concurrently,
    -- which fails because current directory is a shared resource
    -- TODO refactor to remove 'withCurrentDirectory'
      testCase "calls localize function only once on a file change" $ do
        cleanDirectory testDirectory
        withCurrentDirectory testDirectory $ do
          createDirectory "dir"
          let configText = [r|
            {"watchDirs": {
              "dir": "out"
            }}
          |]
          config <- readConfig configText

          callCountVar <- newTVarIO 0
          thread <- forkIO $ startDaemon (fakeLocalizeJSON callCountVar) config

          writeFile "dir/1.json" "{}"
          -- 300 ms to wait for any watched localize calls in response to the file change
          -- it's bigger than the debounce period of 100 ms in 'startDaemon'
          threadDelay 300000

          callCount <- readTVarIO callCountVar
          callCount @?= 1

          killThread thread
  ]

-- | Counts the calls to the 'localizeJSON' function.
fakeLocalizeJSON :: TVar Int -> LocalizeJSON
fakeLocalizeJSON callCount = const . const . void . atomically $ modifyTVar' callCount (+1)

assertDirectoriesExist :: [FilePath] -> Assertion
assertDirectoriesExist = mapM_ $ \path ->
  doesDirectoryExist path @? mconcat ["directory ", path, " does not exist"]

-- | Recreates the directory @dir@ so that it's empty for tests.
cleanDirectory :: FilePath -> IO ()
cleanDirectory dir = do
  exists <- doesPathExist dir
  when exists $ removeDirectoryRecursive dir
  createDirectory dir

-- | Saves the @config@ string into a file to parse it with @parseConfig@
-- and return a @Config@.
readConfig :: String -> IO Config
readConfig config = do
  let configFile = "testconfig.json"
  writeFile configFile config
  parseConfig configFile
