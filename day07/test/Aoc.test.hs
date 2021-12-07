import Aoc (parseInput, solve)
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
  let parsed = parseInput input
  testGroup
    "solveTests"
    [ testCase "should solve part 1" $ assertEqual [] 352997 (solve "part1" parsed),
      testCase "should solve part 2" $ assertEqual [] 101571302 (solve "part2" parsed)
    ]
