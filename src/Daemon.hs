{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Daemon
  ( Config
  , LocalizeJSON

  , localizeJSON
  , parseConfig
  , startDaemon
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, dupChan, newChan, readChan)
import Control.DeepSeq (deepseq)
import Control.Monad (forM_, forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Data.Aeson
import Data.List (isSuffixOf)
import Data.Map.Strict (Map)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics
import System.Directory (doesFileExist, listDirectory, createDirectoryIfMissing, makeRelativeToCurrentDirectory)
import System.Exit (die)
import System.FSNotify
import System.FSNotify.Devel
import System.FilePath ((</>), pathSeparator, replaceDirectory, takeExtension, takeDirectory)
import System.IO (hPutStrLn, stderr)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as C (ByteString, readFile, writeFile)
import qualified Data.Map.Strict as M
import qualified Data.Text as T (unpack)

import JSON (localizeValue)

type WatchDirs = Map FilePath FilePath

-- | Daemon's config.
newtype Config = Config
  { watchDirs :: WatchDirs -- ^ Directories to watch.
  }
  deriving Generic

instance FromJSON Config

-- | Parses @Config@ from @file@. Stops the program on errors.
parseConfig :: FilePath -> IO Config
parseConfig file = do
  exists <- doesFileExist file
  unless exists . die $ "Can't find config file " ++ file

  eitherConfig <- eitherDecodeFileStrict' file
  case eitherConfig of
    Right config -> pure config
    Left err -> die $ "Error parsing config file: " ++ err

-- | In the given directory, watch non-recursively for any 'Added' and 'Modified' events
-- (but ignore 'Removed' events) for files with the given file extension by streaming
-- events to a Chan. This is a non-recursive version of 'System.FSNotify.Devel.treeExtExists'.
dirExtExistsChan :: WatchManager
  -> FilePath -- ^ Directory to watch
  -> Text -- ^ extension
  -> EventChannel -- ^ channel for the file events
  -> IO StopListening
dirExtExistsChan man dir ext =
  watchDirChan man dir (existsEvents $ hasThisExtension ext)

-- | Checks whether the 'FilePath' has the 'ext' extension.
hasThisExtension :: Text -> FilePath -> Bool
hasThisExtension ext = (== T.unpack ext) . takeExtension

type LocalizeJSON = WatchDirs -> FilePath -> IO ()

-- | Starts a daemon that monitors for changes in all json files in the given @watchDirs@ (keys),
-- localizes all the strings in the files on every change, and writes the result to the
-- corresponding @watchDirs@ (values). The @localize@ function is the handler when a file is
-- changed; it's a parameter for mocking in tests.
startDaemon :: LocalizeJSON -> Config -> IO ()
startDaemon localize (Config { watchDirs = watchDirs }) = do
  let dirs = normalizeInputDirs watchDirs
  ensureOutputDirectories dirs
  -- localize the files in the directory at startup to make sure we're up-to-date
  localizeAll dirs

  chan <- newChan
  withManagerConf config $ \mgr -> do
    forM_ (M.keys dirs) $ \dir ->
      dirExtExistsChan mgr dir jsonExt chan

    localizeChan localize dirs chan

  where
    jsonExt :: IsString s => s
    jsonExt = ".json"

    -- increase the debounce interval to 100 ms to filter out quick changes
    -- (caused sometimes by vs code for some reason)
    -- TODO check out the warning at https://www.stackage.org/haddock/lts-19.8/fsnotify-0.3.0.1/System-FSNotify.html#t:Debounce
    config = defaultConfig { confDebounce = Debounce 0.1 }

    -- | Apply @takeDirectory@ to every key in @watchDirs@ to get the same relative path
    -- as is used in @localizeJSON@ below. That is, removes the trailing slash if present.
    normalizeInputDirs = M.mapKeys $ \k -> if [pathSeparator] `isSuffixOf` k
      then takeDirectory k
      else k

    ensureOutputDirectories dirs = forM_ (M.elems dirs) $ createDirectoryIfMissing True

    localizeAll dirs = forM_ (M.toList dirs) $ \(dir, outputDir) -> do
      files <- listDirectory dir
      let jsons = fmap (dir </>) . filter (hasThisExtension jsonExt) $ files
      mapM_ (localize dirs) jsons

-- | Localizes the files for events from @chan@ forever. This is a single-threaded operation for now
-- because it's simpler to implement (no need to keep track if another thread is localizing the same
-- file at the moment).
localizeChan :: LocalizeJSON -> WatchDirs -> EventChannel -> IO ()
localizeChan localize dirs chan = forever $ do
  filepath <- eventPath <$> readChan chan
  localize dirs filepath

-- | Localizes all string values in the given json @file@ and writes the result to a file with
-- the same name in the corresponding directory based on @dirs@.
localizeJSON :: LocalizeJSON
localizeJSON dirs file = do
  err <- runExceptT $ do
    inputDir <- liftIO $ takeDirectory <$> makeRelativeToCurrentDirectory file
    outputDir <- ExceptT . pure $ M.lookup inputDir dirs <??> ("Can't find output directory for " <> file)
    eitherValue <- liftIO $ eitherDecodeFileStrict' file
    case eitherValue of
      Right value -> liftIO $ writeFileIfChanged (replaceDirectory file outputDir) . jq $ localizeValue value
      Left err -> ExceptT . pure . Left . mconcat $ ["Couldn't decode JSON in file ", file, ": ", err]
  either (hPutStrLn stderr) pure err

-- | Annotates the @Nothing@ case with @error@.
(<??>) :: Maybe b -> a -> Either a b
Just x <??> _ = Right x
Nothing <??> error = Left error

-- | Writes @bs@ to the @file@ only if current contents are different.
writeFileIfChanged :: FilePath -> C.ByteString -> IO ()
writeFileIfChanged file bs = do
  exists <- doesFileExist file
  contentsChanged <- if exists
    then (\old -> old `deepseq` old /= bs) <$> C.readFile file
    else pure True
  when contentsChanged $ C.writeFile file bs

-- | Pretty-prints JSON @Value@ as @jq@ does.
jq :: Value -> C.ByteString
jq = AP.encodePretty' jqConfig
  where
    jqConfig :: AP.Config
    jqConfig = AP.Config
      { AP.confIndent = AP.Spaces 2
      , AP.confCompare = compare
      , AP.confNumFormat = AP.Generic
      , AP.confTrailingNewline = True
      }
