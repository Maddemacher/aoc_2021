import Aoc (solve)
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Matrix (fromLists)
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
  let rows = lines input
  let parsed = fromLists (map (map digitToInt) rows)
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 1656 (solve "part1" parsed),
      testCase "should solve part 2" $ assertEqual [] 195 (solve "part2" parsed)
    ]
