module Localize
    ( localize
    ) where

import Data.Char (isLower, toLower, toUpper)

-- | "Localizes" the given string by reversing it and flipping the case
-- of every character (upper<->lower).
localize :: String -> String
localize = reverse . flipCase

-- | Flips the case of the given string.
flipCase :: String -> String
flipCase = map flip
    where flip c = (if isLower c then toUpper else toLower) c
