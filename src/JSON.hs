module JSON
    ( localizeValue
    ) where

import Data.Aeson (Value(..))
import Data.Text (Text)

import Localize (localize)

-- | Localizes all string values in the given JSON @Value@.
localizeValue :: Value -> Value
localizeValue = mapValueStrings localize

-- | Applies @f@ to every string value in the JSON.
mapValueStrings :: (Text -> Text) -> Value -> Value
mapValueStrings f (Object o) = Object $ mapValueStrings f <$> o
mapValueStrings f (Array a) = Array $ mapValueStrings f <$> a
mapValueStrings f (String t) = String $ f t
mapValueStrings _ x = x
