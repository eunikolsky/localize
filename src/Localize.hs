{-# LANGUAGE OverloadedStrings #-}

module Localize
  ( localize
  ) where

import Prelude hiding (concat)

import Data.Char (isLower, toLower, toUpper)
import Data.Function ((&))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text, concat, intercalate, pack, singleton)
import Data.Text.ICU hiding (toLower, toUpper)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar, string)
import qualified Data.Text as T (length)

-- | Type of parsers used here: works on @Text@s and doesn't
-- have any special errors.
type Parser = Parsec Void Text

-- | Represents an individual part of the incoming string.
data InputToken
  = ITokChar Char    -- ^ A char that will be flipped and reversed.
  | ITokString Text  -- ^ An immutable string that will be kept as is.

-- | A list of tokens located between @tokenGroupSeparator@s.
newtype InputTokenGroup = InputTokenGroup { unTokenGroup :: [InputToken] }

-- | The pipe @|@ separator of token groups for PHP.
tokenGroupSeparator :: Char
tokenGroupSeparator = '|'

-- | "Localizes" the given string by reversing it and flipping the case
-- of every character (upper<->lower). Escaped characters (e.g. @\n@, @\"@),
-- PHP-style placeholders (`$foo_BAR`) and React-style placeholders
-- (`{{foo_BAR}}`) are preserved as is.
localize :: Text -> Text
localize t = concat . reverse <$> graphemeClusters t
  ?? (intercalate (singleton tokenGroupSeparator) . fmap processGroup . parseString) t
  where
    graphemeClusters :: Text -> Maybe [Text]
    graphemeClusters t = let chars = brkBreak <$> breaks (breakCharacter Current) t
      in if any isGraphemeCluster chars
        then Just chars
        else Nothing

    isGraphemeCluster :: Text -> Bool
    isGraphemeCluster = (> 1) . T.length

    processGroup :: InputTokenGroup -> Text
    processGroup = concat . fmap flipCase . reverse . unTokenGroup

-- | Parses the input string into a list of token groups.
parseString :: Text -> [InputTokenGroup]
parseString s = parseMaybe (tokenGroup `sepBy` char tokenGroupSeparator) s
  -- I don't expect this parser to fail on any input, but if it does,
  -- leave the input as is.
  ?? [InputTokenGroup [ITokString s]]
  where
    tokenGroup :: Parser InputTokenGroup
    tokenGroup = InputTokenGroup <$> many token

    token :: Parser InputToken
    token
      = ITokString <$> try (choice
        [ jsonEscapedChar
        , phpStylePlaceholder
        , reactStylePlaceholder
        , countPHPPlaceholder
        ])
      <|> ITokChar <$> anySingleBut tokenGroupSeparator

(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? x = x

infixl 3 ??

-- | Flips the case of the given token.
flipCase :: InputToken -> Text
flipCase (ITokChar c) = singleton $ flip c
  where flip c = (if isLower c then toUpper else toLower) c
flipCase (ITokString s) = s

-- | Parses a subset of the allowed escaped characters in JSON.
jsonEscapedChar :: Parser Text
jsonEscapedChar = do
  backslash <- single '\\'
  cont <- oneOf ['t', 'r', 'n', 'b', '"']
  pure . pack $ backslash : [cont]

-- | Parses a PHP-style placeholder, which looks like `$foo` where the
-- allowed characters after `$` are big/small letters and underscore.
phpStylePlaceholder :: Parser Text
phpStylePlaceholder = do
  start <- single '$'
  id <- some $ letterChar <|> single '_'
  pure . pack $ start : id

-- | Parses a React-style placeholder, which looks like `{{foo}}`.
reactStylePlaceholder :: Parser Text
reactStylePlaceholder = do
  start <- string "{{"
  (id, end) <- someTill_ anySingle (string "}}")
  pure $ mconcat [start, pack id, end]

-- | Parses the @%count%@ PHP placeholder for pluralization support.
countPHPPlaceholder :: Parser Text
countPHPPlaceholder = string "%count%"
