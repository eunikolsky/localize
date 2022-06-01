{-# LANGUAGE QuasiQuotes #-}

module DaemonSpec where

import Control.Concurrent
import Control.Monad
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import System.Directory

import Daemon

daemonTests :: TestTree
daemonTests = testGroup "startDaemon"
  [ testCase "creates output directories" $ do
      let testDirectory = "test/daemonspec"
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
        thread <- forkIO $ startDaemon config

        threadDelay 10000 -- TODO is 10 ms enough? refactor to a waiting call
        assertDirectoriesExist ["out", "fake", "fake/en"]

        killThread thread
  ]

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
