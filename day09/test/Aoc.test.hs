import Aoc (getLowPoint, getSurroundingPoints, solve)
import Data.List (sort)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  input <- readFile "data/testInput.txt"
  defaultMain (unitTests input)

unitTests input =
  testGroup
    "Unit tests"
    [ solveTests input
    ]

solveTests :: [Char] -> TestTree
solveTests input = do
  let parsed = lines input
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 15 (solve "part1" parsed),
      testCase "should solve part 2" $ assertEqual [] 10 (solve "part2" parsed),
      testCase "should solve part 2" $ assertEqual [] ['1', '3', '9'] (getSurroundingPoints (0, 0) parsed),
      testCase "should solve part 2" $ assertEqual [] "21938985" (getSurroundingPoints (1, 1) parsed),
      testCase "should solve part 2" $ assertEqual [] Nothing (getLowPoint (1, 1) parsed),
      testCase "should solve part 2" $ assertEqual [] Nothing (getLowPoint (1, 2) parsed)
      -- testCase "should solve part 2" $ assertEqual [] (Just 1) (getLowPoint (1, 0) parsed),
      -- testCase "should solve part 2" $ assertEqual [] (Just 5) (getLowPoint (2, 2) parsed)
    ]
