{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Localize
  ( localize
  ) where

import Prelude hiding (concat)

import Data.Bifunctor (second)
import Data.Char (isLetter, isLower, toLower, toUpper)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (singleton, uncons)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Proxy
import Data.Text (Text, concat, intercalate, pack)
import Data.Text.ICU hiding (span, toLower, toUpper)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, letterChar, string)
import qualified Data.Set as Set
import qualified Data.Text as T (head, length, map, singleton)

-- TODO extract UChar to another module
-- | A single _user-visible_ Unicode character.
data UChar
  = Grapheme Char         -- ^ A single unicode codepoint; most of input will consist only of these.
  | GraphemeCluster Text  -- ^ A sequence of codepoints that represent one user-visible character.
  deriving (Eq, Ord)

mapUChar :: (Char -> Char) -> UChar -> UChar
mapUChar f (Grapheme c) = Grapheme $ f c
mapUChar f (GraphemeCluster t) = GraphemeCluster $ T.map f t

uCharFromBreak :: Text -> UChar
uCharFromBreak t
  | isGraphemeCluster t = GraphemeCluster t
  | T.length t == 1 = Grapheme $ T.head t
  | otherwise = error "tokenFromGraphemeCluster: can't have empty text"

  where
    isGraphemeCluster :: Text -> Bool
    isGraphemeCluster = (> 1) . T.length

uCharToText :: UChar -> Text
uCharToText (Grapheme c) = T.singleton c
uCharToText (GraphemeCluster t) = t

-- | Text containing user-visible Unicode characters (`UChar`s).
newtype UText = UText [UChar]
  deriving (Semigroup, Monoid)

uTextToText :: UText -> Text
uTextToText (UText t) = concat $ fmap uCharToText t

instance Stream UText where
  type Token UText = UChar
  type Tokens UText = [UChar]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null

  take1_ (UText cs) = uncons cs <&> second UText
  takeN_ n s@(UText cs)
    | n <= 0 = Just ([], s)
    | null cs = Nothing
    | otherwise = Just $ splitAt n cs & second UText
  takeWhile_ f (UText cs) = span f cs & second UText

-- `instance VisualStream UText` for debugging isn't implemented
-- `instance TraversableStream UText` for error reporting isn't implemented

-- | Type of parsers used here: works on `UChar`s and doesn't
-- have any special errors.
type Parser = Parsec Void UText

-- | Represents an individual part of the incoming string.
data InputToken
  = ITokChar UChar    -- ^ A char that will be flipped and reversed.
  | ITokString UText  -- ^ An immutable string that will be kept as is.

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
localize = intercalate (T.singleton tokenGroupSeparator) . fmap processGroup . parseString . toUText
  where
    processGroup :: InputTokenGroup -> Text
    processGroup = concat . fmap (uTextToText . flipCase) . reverse . unTokenGroup

toUText :: Text -> UText
toUText = UText . fmap (uCharFromBreak . brkBreak) . breaks (breakCharacter Current)

pChar :: Char -> Parser UChar
pChar = single . Grapheme

pLetterChar :: Parser UChar
pLetterChar =
  -- this implementation is adapted from `megaparsec`
  -- (`Grapheme <$> letterChar` doesn't work)
  satisfy isLetter' <?> "letter"
  where
    isLetter' x@(Grapheme g) = isLetter g
    isLetter' (GraphemeCluster _) = False

pString :: String -> Parser UText
pString = fmap UText . chunk . fmap Grapheme

-- | Parses the input string into a list of token groups.
parseString :: UText -> [InputTokenGroup]
parseString s = parseMaybe (tokenGroup `sepBy` pChar tokenGroupSeparator) s
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
      <|> ITokChar <$> anySingleBut (Grapheme tokenGroupSeparator)

(??) :: Maybe a -> a -> a
Just x ?? _ = x
Nothing ?? x = x

infixl 3 ??

-- | Flips the case of the given token.
flipCase :: InputToken -> UText
flipCase t = case t of
  ITokChar c -> UText . singleton $ flip c
  ITokString s -> s

  where
    flip :: UChar -> UChar
    flip = mapUChar (\c -> (if isLower c then toUpper else toLower) c)

-- | Parses a subset of the allowed escaped characters in JSON.
-- FIXME this should be handled by json parser
jsonEscapedChar :: Parser UText
jsonEscapedChar = do
  backslash <- pChar '\\'
  cont <- oneOf . fmap Grapheme $ ['t', 'r', 'n', 'b', '"']
  pure . UText $ backslash : [cont]

-- | Parses a PHP-style placeholder, which looks like `$foo` where the
-- allowed characters after `$` are big/small letters and underscore.
phpStylePlaceholder :: Parser UText
phpStylePlaceholder = do
  start <- pChar '$'
  id <- some $ pLetterChar <|> pChar '_'
  pure . UText $ start : id

-- | Parses a React-style placeholder, which looks like `{{foo}}`.
reactStylePlaceholder :: Parser UText
reactStylePlaceholder = do
  start <- pString "{{"
  (id, end) <- someTill_ anySingle (pString "}}")
  pure $ mconcat [start, UText id, end]

-- | Parses the @%count%@ PHP placeholder for pluralization support.
countPHPPlaceholder :: Parser UText
countPHPPlaceholder = pString "%count%"
