import Aoc (solve)
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
    [ testCase "should solve part 1" $ assertEqual [] 26397 (solve "part1" parsed),
      testCase "should solve part 2" $ assertEqual [] 288957 (solve "part2" parsed)
    ]
