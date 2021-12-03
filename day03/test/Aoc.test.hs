import Aoc (solve)
import Data.List.Split (splitOn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testNumbers =
  [ "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
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
    [ testCase "should count increases for part1" $ assertEqual [] 198 (solve "part1" testNumbers),
      testCase "should count increases for part1" $ assertEqual [] 230 (solve "part2" testNumbers)
    ]
