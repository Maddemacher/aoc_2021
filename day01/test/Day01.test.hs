import Data.List.Split (splitOn)
import Day01 (generatePairs, solve, splitNumbers)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testNumbers :: [Int]
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
    [ generatePairsTests,
      countIncreasesTests
    ]

generatePairsTests :: TestTree
generatePairsTests =
  testGroup
    "generatePairs"
    [ testCase "should generate pairs" $ assertEqual [] [([1, 2], [2, 3]), ([2, 3], [3, 4]), ([3, 4], [5, 6])] (generatePairs [[1, 2], [2, 3], [3, 4], [5, 6]])
    ]

countIncreasesTests :: TestTree
countIncreasesTests =
  testGroup
    "solveTests"
    [ testCase "should count increases for part1" $ assertEqual [] 7 (solve "part1" testNumbers),
      testCase "should count increases for part1" $ assertEqual [] 5 (solve "part2" testNumbers)
    ]
