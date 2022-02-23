import Test.Tasty
import Test.Tasty.HUnit

import Localize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "localize"
    [ testCase "returns empty string for empty input" $
            localize "" @?= ""

    , testCase "reverses and flips case of the input string" $
            localize "aBcD" @?= "dCbA"

    , testCase "does not change non-cased characters" $
            localize "0_- !?%8" @?= "8%?! -_0"
    ]
