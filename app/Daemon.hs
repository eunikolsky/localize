module Daemon
    ( startDaemon
    ) where

import Control.Monad (unless)
import Data.Aeson
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)
import System.FSNotify
import System.FSNotify.Devel
import System.IO (hPutStrLn, isEOF, stderr)
import qualified Data.ByteString.Lazy.Char8 as C (putStrLn)

import JSON (localizeValue)

-- | Starts a daemon that monitors for changes in the given json @file@
-- and localizes all the strings in the file on every change.
startDaemon :: FilePath -> IO ()
startDaemon file = do
    -- localize the file at startup to make sure we're up-to-date
    localizeJSON file

    withManagerConf config $ \mgr -> do
        canonicalPath <- canonicalizePath file
        -- @watchTree@ works only on directories, so we need to get the @file@'s parent
        let parentDir = takeDirectory canonicalPath
        watchTree mgr parentDir (existsEvents $ isTargetFile canonicalPath) (doAllEvents localizeJSON)

        waitForEOF

    where
        -- we're interested only in @file@ whereas @watchTree@ produces events for
        -- the entire directory
        isTargetFile = (==)

        -- increase the debounce interval to 100 ms to filter out quick changes
        -- (caused sometimes by vs code for some reason)
        -- TODO check out the warning at https://www.stackage.org/haddock/lts-19.8/fsnotify-0.3.0.1/System-FSNotify.html#t:Debounce
        config = defaultConfig { confDebounce = Debounce 0.1 }

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
