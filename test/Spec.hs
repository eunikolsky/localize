import Test.Tasty
import Test.Tasty.HUnit

import Localize

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "localize"
    [
        testCase "returns empty string for empty input" $
            localize "" @?= ""
    ]
