module JSON
  ( LocalizeCache
  , emptyCache
  , localizeValue
  ) where

import Control.Monad.State.Strict
import Data.Aeson (Value(..))
import Data.HashMap.Strict (HashMap, (!?))
import Data.Text (Text)
import qualified Data.HashMap.Strict as M

import Localize (localize)

-- | Type of cache that stores already localized strings for the daemon mode.
type LocalizeCache = HashMap Text Text

-- | Initial, empty cache.
emptyCache :: LocalizeCache
emptyCache = M.empty

-- | Localizes all string values in the given JSON @Value@.
localizeValue :: Value -> State LocalizeCache Value
localizeValue = mapValueStrings cachedLocalize

-- | Localizes 't' using cache: if localized 't' is found in cache, it's
-- returned; otherwise, it's localized, stored in cache and returned.
-- Note: it's an unbounded cache, but it's fine for my use case because I don't
-- expect many additions over the runtime of the process.
cachedLocalize :: Text -> State LocalizeCache Text
cachedLocalize t = do
  cache <- get
  case cache !? t of
    Just v -> pure v
    Nothing -> do
      let v = localize t
      put $ M.insert t v cache
      pure v

-- | Applies @f@ to every string value in the JSON.
mapValueStrings :: Monad m => (Text -> m Text) -> Value -> m Value
mapValueStrings f (Object o) = Object <$> traverse (mapValueStrings f) o
mapValueStrings f (Array a) = Array <$> traverse (mapValueStrings f) a
mapValueStrings f (String t) = String <$> f t
mapValueStrings _ x = pure x
