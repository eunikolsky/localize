module Daemon
    ( startDaemon
    ) where

import Control.Monad (unless)
import System.Directory (canonicalizePath)
import System.FilePath (takeDirectory)
import System.FSNotify
import System.IO (isEOF)

-- | Starts a daemon that monitors for changes in the given json @file@
-- and localizes all the strings in the file on every change.
startDaemon :: FilePath -> IO ()
startDaemon file = withManager $ \mgr -> do
    canonicalPath <- canonicalizePath file
    -- @watchTree@ works only on directories, so we need to get the @file@'s parent
    let parentDir = takeDirectory canonicalPath
    watchTree mgr parentDir (isTargetFile canonicalPath) print

    waitForEOF

    where
        -- we're interested only in @file@ whereas @watchTree@ produces events for
        -- the entire directory
        isTargetFile path ev = eventPath ev == path

-- | Waits until the user hits @Ctlr+d@ (which is @stdin@'s EOF).
waitForEOF :: IO ()
waitForEOF = do
    -- based on https://stackoverflow.com/a/10195290
    finished <- isEOF
    unless finished $ getLine >> waitForEOF
