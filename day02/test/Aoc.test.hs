import Aoc (solve)
import Data.List.Split (splitOn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testNumbers =
  [ ("forward", 5),
    ("down", 5),
    ("forward", 8),
    ("up", 3),
    ("down", 8),
    ("forward", 2)
  ]

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ solveTests
    ]

solveTests :: TestTree
solveTests =
  testGroup
    "solveTests"
    [ testCase "should count increases for part1" $ assertEqual [] 150 (solve "part1" testNumbers),
      testCase "should count increases for part1" $ assertEqual [] 900 (solve "part2" testNumbers)
    ]
