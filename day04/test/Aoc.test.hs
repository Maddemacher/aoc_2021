import Aoc (findWinningBoard, hasWinningRow, parseInput, solve, sublist)
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
  input2 <- readFile "data/testInput2.txt"
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
    [ testCase "should find winning board" $ assertEqual [] True (hasWinningRow untilWinningRow winningBoard),
      testCase "should not find losing board" $ assertEqual [] False (hasWinningRow (init untilWinningRow) winningBoard),
      testCase "should return true on sublist" $ assertEqual [] True (sublist [2, 3] [1, 2, 3]),
      testCase "should return false on not sublist" $ assertEqual [] False (sublist [3, 4] [1, 2, 3]),
      testCase "should return false on not sublist" $ assertEqual [] 102 (solve "part1" (numbers2, boards2)),
      testCase "should count increases for part1" $ assertEqual [] 4512 (solve "part1" (numbers, boards))
      -- testCase "should count increases for part1" $ assertEqual [] 230 (solve "part2" testNumbers)
    ]
