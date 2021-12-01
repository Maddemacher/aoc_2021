import Data.List.Split (splitOn)
import Day01 (countIncreases)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testNumbers :: Integral a => [a]
testNumbers =
  [ 199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263
  ]

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ countIncreasesTests
    ]

countIncreasesTests :: TestTree
countIncreasesTests =
  testGroup
    "countIncreasesTests"
    [ testCase "should count increases" $ assertEqual [] 7 (countIncreases testNumbers)
    ]
