{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Aeson
import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit
import Text.RawString.QQ (r)
import qualified Data.ByteString.Lazy as BL

import JSON (localizeValue)
import Localize (localize)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [localizeTests, localizeValueTests]

localizeTests :: TestTree
localizeTests = testGroup "localize"
  [ testCase "returns empty string for empty input" $
      localize "" @?= ""

  , testCase "reverses and flips case of the input string" $
      localize "aBcD" @?= "dCbA"

  , testCase "does not change non-cased characters" $
      localize "0_- !?%8" @?= "8%?! -_0"

  , testCase "preserves escaped characters" $
      localize "\\tHello\\r\\n \\\"World\\\"\\b\\\\" @?= "\\\\\\b\\\"DLROw\\\" \\n\\rOLLEh\\t"

  , testCase "preserves PHP-style placeholders" $
      localize "Hello ($wor_LD\\\") $y" @?= "$y )\\\"$wor_LD( OLLEh"

  , testCase "preserves React-style placeholders" $
      localize "Hello {{wor_LD}}\\\" {{count}}" @?= "{{count}} \\\"{{wor_LD}} OLLEh"

  , testCase "ignores incomplete React-style placeholders" $
      localize "Hello {{wor_{{LD}}\\\" {{count" @?= "TNUOC{{ \\\"{{wor_{{LD}} OLLEh"

  , testCase "preserves unicode characters" $
      localize "ёHello world \\\"$xyz\\\" ёЁ ❓🚜 й ❄" @?= "❄ Й 🚜❓ ёЁ \\\"$xyz\\\" DLROW OLLEhЁ"

  , testCase "preserves %count% PHP placeholders" $
      localize "Hello %count% world" @?= "DLROW %count% OLLEh"

  , testCase "keeps the order of groups separated by pipe" $
      localize "Hello ONE world? $foo|Hello %count% worlds!|other" @?= "$foo ?DLROW eno OLLEh|!SDLROW %count% OLLEh|REHTO"
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
