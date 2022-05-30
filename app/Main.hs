module Main where

import Data.Text.IO as TIO (interact)
import Data.Version (showVersion)
import System.Environment (getArgs)

import Paths_localize (version)
import Daemon (parseConfig, startDaemon)
import Localize (localize)

type ConfigFilePath = FilePath

-- | Possible actions for the program, controlled by command line arguments.
data Action
  = LocalizeInput
  | StartDaemon ConfigFilePath
  | PrintVersion

run :: Action -> IO ()
run LocalizeInput = TIO.interact localize
run (StartDaemon configFile) = parseConfig configFile >>= startDaemon
run PrintVersion = putStrLn $ showVersion version

main :: IO ()
main = do
  args <- getArgs
  let { action = case args of
    [] -> LocalizeInput
    ["-d", configFile] -> StartDaemon configFile
    ["--version"] -> PrintVersion
    _ -> error "Unrecognized arguments"
  }
  run action
