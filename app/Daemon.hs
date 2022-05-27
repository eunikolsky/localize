{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Daemon
  ( parseConfig
  , startDaemon
  ) where

import Control.Monad (forM_, unless)
import Data.Aeson
import Data.String (IsString)
import GHC.Generics
import System.Directory (doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath ((</>), takeExtension)
import System.FSNotify
import System.FSNotify.Devel
import System.IO (hPutStrLn, isEOF, stderr)
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn)

import JSON (localizeValue)

-- | Daemon's config.
newtype Config = Config
  { watchDirs :: [FilePath] -- ^ Directories to watch.
  }
  deriving Generic

instance FromJSON Config

-- | Parses @Config@ from @file@. Stops the program on errors.
parseConfig :: FilePath -> IO Config
parseConfig file = do
  exists <- doesFileExist file
  unless exists . die $ "Can't find config file: " ++ file

  eitherConfig <- eitherDecodeFileStrict' file
  case eitherConfig of
    Right config -> pure config
    Left err -> die $ "Error parsing config file: " ++ err

-- | Starts a daemon that monitors for changes in all json files in the given @watchDirs@
-- and localizes all the strings in the files on every change.
startDaemon :: Config -> IO ()
startDaemon (Config { watchDirs = dirs }) = do
  -- localize the files in the directory at startup to make sure we're up-to-date
  localizeAll

  -- FIXME this produces duplicate events for files in a subdirectory when both it and its parent
  -- directory are being watched.
  withManagerConf config $ \mgr -> do
    forM_ dirs $ \dir -> treeExtExists mgr dir jsonExt localizeJSON

    waitForEOF

  where
    jsonExt :: IsString s => s
    jsonExt = ".json"

    -- increase the debounce interval to 100 ms to filter out quick changes
    -- (caused sometimes by vs code for some reason)
    -- TODO check out the warning at https://www.stackage.org/haddock/lts-19.8/fsnotify-0.3.0.1/System-FSNotify.html#t:Debounce
    config = defaultConfig { confDebounce = Debounce 0.1 }

    localizeAll = forM_ dirs $ \dir -> do
      files <- listDirectory dir
      let jsons = fmap (dir </>) . filter ((== jsonExt) . takeExtension) $ files
      mapM_ localizeJSON jsons

-- | Waits until the user hits @Ctlr+d@ (which is @stdin@'s EOF).
waitForEOF :: IO ()
waitForEOF = do
  -- based on https://stackoverflow.com/a/10195290
  finished <- isEOF
  unless finished $ getLine >> waitForEOF

-- | Localizes all string values in the given @json@ file.
localizeJSON :: FilePath -> IO ()
localizeJSON file = do
  eitherValue <- eitherDecodeFileStrict' file
  case eitherValue of
    Right value -> C.putStrLn . encode $ localizeValue value
    Left err -> hPutStrLn stderr err
