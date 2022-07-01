module DaemonSpec where

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.STM
import Data.Bifunctor
import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.FilePath

import Daemon

-- | Root directory where per-test case subdirectories are created.
testRoot :: FilePath
testRoot = "test/daemonspec"

daemonTests :: TestTree
daemonTests = testGroup "startDaemon"
  [ testCase "creates output directories" $ do
      let testDirectory = testRoot </> "creates-output-directories"
      cleanDirectory testDirectory
      let mapping = fmap (bimap (testDirectory </>) (testDirectory </>)) [("dir", "out"), ("lang", "fake"), ("lang/en", "fake/en")]
      mapM_ (createDirectory . fst) mapping
      let { configText = "{\"watchDirs\": {"
        <> (intercalate "," . fmap (\(src, dest) -> wrapInQuotes src <> ":" <> wrapInQuotes dest) $ mapping)
        <> "}}"
      }
      config <- readConfig testDirectory configText
      thread <- forkIO $ startDaemon localizeJSON config

      threadDelay 10000 -- TODO is 10 ms enough? refactor to a waiting call
      assertDirectoriesExist $ snd <$> mapping

      killThread thread

  , testCase "calls localize function only once on a file change" $ do
      let testDirectory = testRoot </> "calls-localize-once"
      cleanDirectory testDirectory
      let mapping = (testDirectory </> "dir", testDirectory </> "out")
      createDirectory $ fst mapping
      let { configText = "{\"watchDirs\": {"
        <> wrapInQuotes (fst mapping) <> ":" <> wrapInQuotes (snd mapping)
        <> "}}"
      }
      config <- readConfig testDirectory configText

      callCountVar <- newTVarIO 0
      thread <- forkIO $ startDaemon (fakeLocalizeJSON callCountVar) config

      -- note: this initial delay is needed to skip the initial localization
      threadDelay 1000
      writeFile (testDirectory </> "dir/1.json") "{}"
      -- 300 ms to wait for any watched localize calls in response to the file change
      -- it's bigger than the debounce period of 100 ms in 'startDaemon'
      threadDelay 300000

      callCount <- readTVarIO callCountVar
      callCount @?= 1

      killThread thread

  , testGroup "when nested directories are watched"
      [ testCase "calls localize function only once on a file change" $ do
          let testDirectory = testRoot </> "nested_calls-localize-once"
          cleanDirectory testDirectory
          let mapping = fmap (bimap (testDirectory </>) (testDirectory </>)) [("dir", "out"), ("dir/nested", "out/nested")]
          mapM_ (createDirectory . fst) mapping
          let { configText = "{\"watchDirs\": {"
            <> (intercalate "," . fmap (\(src, dest) -> wrapInQuotes src <> ":" <> wrapInQuotes dest) $ mapping)
            <> "}}"
          }
          config <- readConfig testDirectory configText

          callCountVar <- newTVarIO 0
          thread <- forkIO $ startDaemon (fakeLocalizeJSON callCountVar) config

          -- note: this initial delay is needed to skip the initial localization
          threadDelay 1000
          writeFile (testDirectory </> "dir/nested/1.json") "[]"
          -- 300 ms to wait for any watched localize calls in response to the file change
          -- it's bigger than the debounce period of 100 ms in 'startDaemon'
          threadDelay 500000 -- FIXME refactor to a polling call

          callCount <- readTVarIO callCountVar
          callCount @?= 1

          killThread thread
      ]
  ]

wrapInQuotes :: String -> String
wrapInQuotes s = "\"" ++ s ++ "\""

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
  let createParents = True
  createDirectoryIfMissing createParents dir

-- | Saves the @config@ string into a file in @dir@ to parse it with @parseConfig@
-- and return a @Config@.
readConfig :: FilePath -> String -> IO Config
readConfig dir config = do
  let configFile = dir </> "testconfig.json"
  writeFile configFile config
  parseConfig configFile
