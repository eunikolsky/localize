{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Daemon
  ( parseConfig
  , startDaemon
  ) where

import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Map.Strict (Map)
import Data.String (IsString)
import GHC.Generics
import System.Directory (doesFileExist, listDirectory, createDirectoryIfMissing)
import System.Exit (die)
import System.FSNotify
import System.FSNotify.Devel
import System.FilePath ((</>), replaceDirectory, takeDirectory, takeExtension)
import System.IO (hPutStrLn, isEOF, stderr)
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.ByteString.Lazy.Char8 as C (ByteString, writeFile)
import qualified Data.Map.Strict as M

import JSON (localizeValue)

-- | Daemon's config.
newtype Config = Config
  { watchDirs :: Map FilePath FilePath -- ^ Directories to watch.
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

-- | Starts a daemon that monitors for changes in all json files in the given @watchDirs@ (keys),
-- localizes all the strings in the files on every change, and writes the result to the
-- corresponding @watchDirs@ (values).
startDaemon :: Config -> IO ()
startDaemon (Config { watchDirs = dirs }) = do
  -- localize the files in the directory at startup to make sure we're up-to-date
  localizeAll

  -- FIXME this produces duplicate events for files in a subdirectory when both it and its parent
  -- directory are being watched.
  withManagerConf config $ \mgr -> do
    forM_ (M.toList dirs) $
      \(dir, outputDir) -> treeExtExists mgr dir jsonExt (localizeJSON outputDir)

    waitForEOF

  where
    jsonExt :: IsString s => s
    jsonExt = ".json"

    -- increase the debounce interval to 100 ms to filter out quick changes
    -- (caused sometimes by vs code for some reason)
    -- TODO check out the warning at https://www.stackage.org/haddock/lts-19.8/fsnotify-0.3.0.1/System-FSNotify.html#t:Debounce
    config = defaultConfig { confDebounce = Debounce 0.1 }

    localizeAll = forM_ (M.toList dirs) $ \(dir, outputDir) -> do
      files <- listDirectory dir
      let jsons = fmap (dir </>) . filter ((== jsonExt) . takeExtension) $ files
      mapM_ (localizeJSON outputDir) jsons

-- | Waits until the user hits @Ctlr+d@ (which is @stdin@'s EOF).
waitForEOF :: IO ()
waitForEOF = do
  -- based on https://stackoverflow.com/a/10195290
  finished <- isEOF
  unless finished $ getLine >> waitForEOF

-- | Localizes all string values in the given json @file@ and writes the result to a file with
-- the same name in @outputDir@.
localizeJSON :: String -> FilePath -> IO ()
localizeJSON outputDir file = do
  eitherValue <- eitherDecodeFileStrict' file
  case eitherValue of
    Right value -> writeFile' (replaceDirectory file outputDir) . jq $ localizeValue value
    Left err -> hPutStrLn stderr err

-- | Writes @ByteString@ to @file@ ensuring that its directory exists.
writeFile' :: FilePath -> C.ByteString -> IO ()
writeFile' file bs = do
  createDirectoryIfMissing True $ takeDirectory file
  C.writeFile file bs

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
