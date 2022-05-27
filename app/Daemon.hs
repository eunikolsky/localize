{-# LANGUAGE OverloadedStrings #-}

module Daemon
    ( startDaemon
    ) where

import Control.Monad (unless)
import Data.Aeson
import Data.String (IsString)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.FSNotify
import System.FSNotify.Devel
import System.IO (hPutStrLn, isEOF, stderr)
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn)

import JSON (localizeValue)

-- | Starts a daemon that monitors for changes in all json files in the given @dir@
-- and localizes all the strings in the files on every change.
startDaemon :: FilePath -> IO ()
startDaemon dir = do
    -- localize the files in the directory at startup to make sure we're up-to-date
    localizeAll

    withManagerConf config $ \mgr -> do
        treeExtExists mgr dir jsonExt localizeJSON

        waitForEOF

    where
        jsonExt :: IsString s => s
        jsonExt = ".json"

        -- increase the debounce interval to 100 ms to filter out quick changes
        -- (caused sometimes by vs code for some reason)
        -- TODO check out the warning at https://www.stackage.org/haddock/lts-19.8/fsnotify-0.3.0.1/System-FSNotify.html#t:Debounce
        config = defaultConfig { confDebounce = Debounce 0.1 }

        localizeAll = do
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
