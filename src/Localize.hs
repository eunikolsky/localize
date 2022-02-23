module Localize
    ( localize
    ) where

import Data.Char (isLower, toLower, toUpper)
import Data.Function ((&))
import Data.Void (Void)
import Text.Megaparsec hiding (Token)
import Data.Maybe (fromMaybe)

-- | Type of parsers used here: works on regular @String@s and doesn't
-- have any special errors.
type Parser = Parsec Void String

-- | Represents an individual part of the incoming string.
data Token
    = TokChar Char     -- ^ A char that will be flipped and reversed.
    | TokString String -- ^ An immutable string that will be kept as is.

-- | "Localizes" the given string by reversing it and flipping the case
-- of every character (upper<->lower). Escaped characters are preserved
-- as is (e.g. @\n@).
localize :: String -> String
localize = concatMap flipCase . reverse . parseString

-- | Parses the input string into a list of tokens.
parseString :: String -> [Token]
parseString s = parseMaybe (many token) s
    -- I don't expect this parser to fail on any input, but if it does,
    -- split the string on characters.
    & or (dumbSplit s)
    where
        token :: Parser Token
        token
            = TokString <$> try jsonEscapedChar
            <|> TokChar <$> anySingle
        or = fromMaybe
        dumbSplit = fmap TokChar

-- | Flips the case of the given token.
flipCase :: Token -> String
flipCase (TokChar c) = [flip c]
    where flip c = (if isLower c then toUpper else toLower) c
flipCase (TokString s) = s

-- | Parses a subset of the allowed escaped characters in JSON.
jsonEscapedChar :: Parser String
jsonEscapedChar = do
    backslash <- single '\\'
    cont <- oneOf ['t', 'r', 'n', 'b', '"']
    pure $ backslash : [cont]
