import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [
        testCase "test" $ reverse [1, 2, 3] @?= [3, 2, 1]
    ]
