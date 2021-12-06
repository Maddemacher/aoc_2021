import Aoc (parseInput, simulate, simulateIterations, solve)
import Data.List (sort)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  testInput <- readFile "data/testInput.txt"
  defaultMain (unitTests input testInput)

unitTests input testInput =
  testGroup
    "Unit tests"
    [ solveTests input testInput
    ]

solveTests :: [Char] -> [Char] -> TestTree
solveTests input testInput = do
  let parsed = parseInput input
  let parsedTest = parseInput testInput
  testGroup
    "solveTests"
    [ testCase "should solve part 1 testinput" $ assertEqual [] 26 (sum (simulateIterations parsedTest 18)),
      testCase "should solve part 1" $ assertEqual [] 380243 (solve "part1" parsed),
      testCase "should solve part 1" $ assertEqual [] 1708791884591 (solve "part2" parsed)
    ]
