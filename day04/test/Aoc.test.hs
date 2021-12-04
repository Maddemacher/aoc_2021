import Aoc (hasWinningRow, parseInput, solve, sublist)
import Data.List.Split (splitOn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

untilWinningRow = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24]

winningBoard =
  [ [14, 21, 17, 24, 4],
    [10, 16, 15, 9, 19],
    [18, 8, 23, 26, 20],
    [22, 11, 13, 6, 5],
    [2, 0, 12, 3, 7]
  ]

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
  let (numbers, boards) = (parseInput input)
  let (numbers2, boards2) = (parseInput input2)
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 4512 (solve "part1" numbers boards),
      testCase "should solve part 1 with real data" $ assertEqual [] 14093 (solve "part1" numbers2 boards2),
      testCase "should sovle part 2 with real data" $ assertEqual [] 17388 (solve "part2" numbers2 boards2)
    ]
