module Main where

import Data.Text.IO as TIO (interact)
import Data.Version (showVersion)
import System.Environment (getArgs)

import Paths_localize (version)
import Daemon (parseConfig, startDaemon)
import Localize (localize)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> TIO.interact localize
    ["-d", configFile] -> parseConfig configFile >>= startDaemon
    ["--version"] -> putStrLn $ showVersion version
    _ -> error "Unrecognized arguments"
