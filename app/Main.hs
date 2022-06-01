module Main where

import Data.Foldable (asum)
import Data.Text.IO as TIO (interact)
import Data.Version (showVersion)
import Options.Applicative hiding (action)

import Paths_localize (version)
import Daemon (parseConfig, startDaemon)
import Localize (localize)
import Options.Applicative.Types (optShowDefault)

type ConfigFilePath = FilePath

-- | Possible actions for the program, controlled by command line arguments.
data Action
  = LocalizeInput
  | StartDaemon ConfigFilePath
  | PrintVersion

action :: Parser Action
action = asum [startDaemon, printVersion, localizeInput]
  where
    startDaemon = StartDaemon <$> strOption (short 'd' <> metavar "CONFIG_FILE" <> help "Start the daemon watching files")
    printVersion = PrintVersion <$ flag' () (long "version" <> help "Print version")
    localizeInput = pure LocalizeInput

run :: Action -> IO ()
run LocalizeInput = TIO.interact localize
run (StartDaemon configFile) = parseConfig configFile >>= startDaemon
run PrintVersion = putStrLn $ showVersion version

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (action <**> helper)
      ( fullDesc
     <> header "Fake localization helper tool" )
