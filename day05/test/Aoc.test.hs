import Aoc (Line, countDuplicatePoints, parseInput, solve, toPointLine, toUsedPoints)
import Data.List.Split (splitOn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testLines =
  [ ((0, 0), (0, 2)),
    ((0, 0), (0, 2)),
    ((0, 0), (2, 0))
  ]

testLinesToPoints =
  [ (0, 0), -- l1
    (0, 1), -- l1
    (0, 2), -- l1
    (0, 0), -- l2
    (0, 1), -- l2
    (0, 2), -- l2
    (0, 0), -- l3
    (1, 0), -- l3
    (2, 0) -- l3
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
  let parsed = parseInput input
  let parsed2 = parseInput input2
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 5 (solve "part1" parsed),
      testCase "should solve part 1" $ assertEqual [] 12 (solve "part2" parsed),
      testCase "vertical to point line" $ assertEqual [] [(0, 0), (0, 1), (0, 2)] (toPointLine ((0, 0), (0, 2))),
      testCase "diagonal to point line" $ assertEqual [] [(0, 0), (1, 1), (2, 2)] (toPointLine ((0, 0), (2, 2))),
      testCase "diagonal to point line" $ assertEqual [] [(9, 7), (8, 8), (7, 9)] (toPointLine ((9, 7), (7, 9))),
      testCase "horizontal to point line" $ assertEqual [] [(0, 0), (1, 0), (2, 0)] (toPointLine ((0, 0), (2, 0))),
      testCase "horizontal to point line" $ assertEqual [] testLinesToPoints (toUsedPoints testLines)
    ]
