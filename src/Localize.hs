{-# LANGUAGE OverloadedStrings #-}

module Localize
    ( localize
    ) where

import Prelude hiding (concat)

import Data.Char (isLower, toLower, toUpper)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Text (Text, concat, intercalate, pack, singleton)
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char (char, letterChar, string)

-- | Type of parsers used here: works on @Text@s and doesn't
-- have any special errors.
type Parser = Parsec Void Text

-- | Represents an individual part of the incoming string.
data Token
    = TokChar Char    -- ^ A char that will be flipped and reversed.
    | TokString Text  -- ^ An immutable string that will be kept as is.

-- | A list of tokens located between @tokenGroupSeparator@s.
newtype TokenGroup = TokenGroup { unTokenGroup :: [Token] }

-- | The pipe @|@ separator of token groups for PHP.
tokenGroupSeparator :: Char
tokenGroupSeparator = '|'

-- | "Localizes" the given string by reversing it and flipping the case
-- of every character (upper<->lower). Escaped characters (e.g. @\n@, @\"@),
-- PHP-style placeholders (`$foo_BAR`) and React-style placeholders
-- (`{{foo_BAR}}`) are preserved as is.
localize :: Text -> Text
localize = intercalate (singleton tokenGroupSeparator) . fmap processGroup . parseString
    where
        processGroup :: TokenGroup -> Text
        processGroup = concat . fmap flipCase . reverse . unTokenGroup

-- | Parses the input string into a list of token groups.
parseString :: Text -> [TokenGroup]
parseString s = parseMaybe (tokenGroup `sepBy` char tokenGroupSeparator) s
    -- I don't expect this parser to fail on any input, but if it does,
    -- leave the input as is.
    & or [TokenGroup [TokString s]]
    where
        tokenGroup :: Parser TokenGroup
        tokenGroup = TokenGroup <$> many token

        token :: Parser Token
        token
            = TokString <$> try (choice
                [ jsonEscapedChar
                , phpStylePlaceholder
                , reactStylePlaceholder
                , countPHPPlaceholder
                ])
            <|> TokChar <$> anySingleBut tokenGroupSeparator

        or = fromMaybe

-- | Flips the case of the given token.
flipCase :: Token -> Text
flipCase (TokChar c) = singleton $ flip c
    where flip c = (if isLower c then toUpper else toLower) c
flipCase (TokString s) = s

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
