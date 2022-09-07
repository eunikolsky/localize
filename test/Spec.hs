{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson
import Data.List (unwords)
import Data.Maybe (fromJust)
import Data.String (IsString(..))
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf (printf)
import Text.RawString.QQ (r)
import Text.Show.Unicode (ushow)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

import JSON (localizeValue)
import Localize (localize)
import DaemonSpec (daemonTests)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [localizeTests, localizeValueTests, daemonTests]

localizeTests :: TestTree
localizeTests = testGroup "localize"
  [ testCase "returns empty string for empty input" $
      localize "" @?= ""

  , testCase "reverses and flips case of the input string" $
      localize "aBcD" @?= "dCbA"

  , testCase "does not change non-cased characters" $
      localize "0_- !?%8" @?= "8%?! -_0"

  , testCase "preserves escaped characters" $
      localize [r|\tHello\r\n \"World\"\b\\|] @?= [r|\\\b\"DLROw\" \n\rOLLEh\t|]

  , testCase "preserves PHP-style placeholders" $
      localize [r|Hello ($wor_LD\") $y|] @?= [r|$y )\"$wor_LD( OLLEh|]

  , testCase "preserves React-style placeholders" $
      localize [r|Hello {{wor_LD}}\" {{count}}|] @?= [r|{{count}} \"{{wor_LD}} OLLEh|]

  , testCase "preserves React-style placeholders with unescaping" $
      localize [r|Hello {{- wor_LD}}\" {{- count}}|] @?= [r|{{- count}} \"{{- wor_LD}} OLLEh|]

  , testCase "ignores incomplete React-style placeholders" $
      localize [r|Hello {{wor_{{LD}}\" {{count|] @?= [r|TNUOC{{ \"{{wor_{{LD}} OLLEh|]

  , testCase "preserves unicode characters" $
      localize [r|ёHello world \"$xyz\" ёЁ ❓🚜 й ❄|] @?= [r|❄ Й 🚜❓ ёЁ \"$xyz\" DLROW OLLEhЁ|]

  , testCase "preserves %count% PHP placeholders" $
      localize "Hello %count% world" @?= "DLROW %count% OLLEh"

  , testCase "keeps the order of groups separated by pipe" $
      localize "Hello ONE world? $foo|Hello %count% worlds!|other" @?= "$foo ?DLROW eno OLLEh|!SDLROW %count% OLLEh|REHTO"

  , graphemeClustersSupportTests
  ]

-- | A better presentation of unicode `Text`. It prints the unicode string as is (although
-- in quotes) and the codepoint of every character instead of escaped unicode characters.
newtype UnicodePrintingText = UnicodePrintingText Text
  deriving Eq

instance Show UnicodePrintingText where
  -- it seems weird to me that I have to use the `ushow` function to display a unicode string as is;
  -- both `String` and `Text` have the `Show` instance that escapes unicode characters; the only way
  -- to print the string as is is `putStrLn` or `Data.Text.IO.putStrLn`, but those are `IO` and not
  -- applicable here
  show (UnicodePrintingText t) = ushow t <> codepoints t
    where
      codepoints = (" (" <>) . (<> ")") . unwords . fmap toCodepoint . T.unpack
      toCodepoint = ("U+" <>) . printf "%04X"

instance IsString UnicodePrintingText where
  fromString = UnicodePrintingText . fromString

localize' :: Text -> UnicodePrintingText
localize' = UnicodePrintingText . localize

-- TODO benchmark
graphemeClustersSupportTests :: TestTree
graphemeClustersSupportTests = testGroup "when grapheme clusters are present"
  [ testCase "returns a single grapheme cluster as is" $
      localize' "❄️" @?= "❄️"

  , testCase "reverses the order of multiple grapheme clusters" $
      localize' "❄️~̲♥︎" @?= "♥︎~̲❄️"

  , testCase "flips case of grapheme clusters" $
      localize' "❄️á♥︎E̺͆" @?= "e̺͆♥︎Á❄️"

  , testCase "reverses and flips case of graphemes" $
      localize' "a❄️ ~̲X0 ♥︎_D" @?= "d_♥︎ 0x~̲ ❄️A"

  , testCase "preserves escaped characters" $
      localize' [r|\tHello❄️\r\n \"áWorld\"\b♥︎\\|] @?= [r|\\♥︎\b\"DLROwÁ\" \n\r❄️OLLEh\t|]

  , testCase "preserves PHP-style placeholders" $
      localize [r|Hello ❄️($wor_LD\"á) $y|] @?= [r|$y )Á\"$wor_LD(❄️ OLLEh|]

  , testCase "preserves React-style placeholders" $
      localize [r|ÁHello {{wor_LD}}\" ♥︎{{count}}|] @?= [r|{{count}}♥︎ \"{{wor_LD}} OLLEhá|]

  , testCase "preserves React-style placeholders with unescaping" $
      localize [r|❄️Hello {{- wor_LD}}\" ♥︎{{- count}}|] @?= [r|{{- count}}♥︎ \"{{- wor_LD}} OLLEh❄️|]

  , testCase "ignores incomplete React-style placeholders" $
      localize [r|Hello❄️ {{wor_{{LD}}\" á{{count|] @?= [r|TNUOC{{Á \"{{wor_{{LD}} ❄️OLLEh|]

  , testCase "preserves %count% PHP placeholders" $
      localize "Hello á%count%♥︎ world" @?= "DLROW ♥︎%count%Á OLLEh"

  , testCase "keeps the order of groups separated by pipe" $
      localize "Hello ONE world?❄️ $foo|áHello %count% worlds!|othÁer♥︎" @?= "$foo ❄️?DLROW eno OLLEh|!SDLROW %count% OLLEhÁ|♥︎REáHTO"

-- TODO test these:
--"👩‍💻🏴‍☠️🏳️‍🌈👩‍🚀"
--"🤦🏼‍♂️"
  ]

localizeValueTests :: TestTree
localizeValueTests = testGroup "localizeValue"
  [ testCase "returns empty value for empty value" $
      localizeValue (forceDecode "{}") @?= forceDecode "{}"

  , testCase "localizes all string values" $
      localizeValue (forceDecode [r|
          { "foo": "Foo", "bar": "BAR",
            "array": [ "Hello", "World!" ],
            "nested": { "object": { "here": "Object" } }
          }
      |]) @?= forceDecode [r|
          { "foo": "OOf", "bar": "rab",
            "array": [ "OLLEh", "!DLROw" ],
            "nested": { "object": { "here": "TCEJBo" } }
          }
      |]

  , testCase "localizes raw string" $
    localizeValue (forceDecode [r|"foo BAR"|]) @?= forceDecode [r|"rab OOF"|]

  , testCase "preserves non-string types" $
    localizeValue (forceDecode [r|
        { "foo": 3.1415,
          "bar": "bar",
          "array": [ "HELLO", true ],
          "nested": { "object": { "here": null }, "string": "str" }
        }
    |]) @?= forceDecode [r|
        { "foo": 3.1415,
          "bar": "RAB",
          "array": [ "olleh", true ],
          "nested": { "object": { "here": null }, "string": "RTS" }
        }
    |]
  ]

forceDecode :: BL.ByteString -> Value
forceDecode = fromJust . decode
