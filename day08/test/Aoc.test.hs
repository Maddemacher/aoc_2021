import Aoc (solve)
import Data.List (sort)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  defaultMain (unitTests input)

unitTests input =
  testGroup
    "Unit tests"
    [ solveTests input
    ]

solveTests :: [Char] -> TestTree
solveTests input = do
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 421 (solve "part1" input),
      testCase "should solve part 2" $ assertEqual [] 986163 (solve "part2" input)
    ]
