import Aoc (parseInput, solve)
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
    [ testCase "should solve part 1" $ assertEqual [] 656 (solve "part1" parsed),
      testCase "should solve part 2" $ assertEqual [] 2979 (solve "part2" parsed)
    ]
