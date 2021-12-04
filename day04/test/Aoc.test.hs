import Aoc (hasWinningRow, parseInput, solve, sublist)
import Data.List.Split (splitOn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  input <- readFile "data/testInput.txt"
  input2 <- readFile "data/input.txt"
  defaultMain (unitTests input input2)

unitTests input input2 =
  testGroup
    "Unit tests"
    [ solveTests input input2
    ]

solveTests input input2 = do
  let parsed = parseInput input
  let parsed2 = parseInput input2
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 4512 (solve "part1" parsed),
      testCase "should solve part 1 with real data" $ assertEqual [] 14093 (solve "part1" parsed2),
      testCase "should sovle part 2 with real data" $ assertEqual [] 17388 (solve "part2" parsed2)
    ]
