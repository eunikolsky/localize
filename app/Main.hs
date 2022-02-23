module Main where

import Data.Text.IO as TIO
import Localize (localize)

main :: IO ()
main = TIO.interact localize
