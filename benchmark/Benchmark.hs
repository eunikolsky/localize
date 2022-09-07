{-# language TypeApplications #-}

module Main where

import Control.Monad (replicateM)
import Criterion
import Criterion.Main (defaultMain)
import Data.Functor
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import System.Random

import Localize
import qualified SimpleLocalize as Sim

main :: IO ()
main = do
  s100 <- randomString 100
  s1000 <- randomString 1000
  s10000 <- randomString 10000

  defaultMain
    [ bgroup "simple localize tests"
      [ bench "Size 100" $ whnf Sim.localize s100
      , bench "Size 1000" $ whnf Sim.localize s1000
      , bench "Size 10000" $ whnf Sim.localize s10000
      ]

    , bgroup "extended localize tests"
      [ bench "Size 100" $ whnf localize s100
      , bench "Size 1000" $ whnf localize s1000
      , bench "Size 10000" $ whnf localize s10000
      ]
    ]

randomString :: Int -> IO Text
randomString n = T.pack . concat <$> replicateM n randomToken
  where
    randomToken = do
      picker <- randomRIO @Int (0, 5)
      case picker of
        -- FIXME the cases should be weighted
        0 -> singleton <$> randomRIO ('a', 'z')
        1 -> singleton <$> randomRIO ('A', 'Z')
        2 -> singleton <$> randomRIO ('0', '9')
        3 -> randomIdentifier <&> \id -> " {{" <> id <> "}} "
        4 -> randomIdentifier <&> \id -> " $" <> id <> " "
        5 -> pure "%count%"
        _ -> error "impossible"

    randomIdentifier = do
      length <- randomRIO (1, 16)
      replicateM length $ do
        picker <- randomRIO @Int (0, 2)
        case picker of
          0 -> randomRIO ('a', 'z')
          1 -> randomRIO ('A', 'Z')
          2 -> pure '_'
          _ -> error "impossible"
