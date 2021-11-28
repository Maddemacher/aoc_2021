import Data.List.Split (splitOn)
import Day01 (divisors, doPart, prime)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

testNumbers = [0, 3, 4, 42, 106, 107, 267, 269]

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [ divisorsTests,
      primeTests,
      partTests
    ]

divisorsTests :: TestTree
divisorsTests =
  testGroup
    "divisors"
    [ testCase "should return divisors of 0" $ assertEqual [] [1] (divisors 0),
      testCase "should return divisors of 1" $ assertEqual [] [1] (divisors 1),
      testCase "should return divisors of 10" $ assertEqual [] [1, 2, 5, 10] (divisors 10)
    ]

primeTests :: TestTree
primeTests =
  testGroup
    "prime"
    [ testCase "should return false for 0" $ assertEqual [] False (prime 0),
      testCase "should return false for 1" $ assertEqual [] False (prime 1),
      testCase "should return true for prime" $ assertEqual [] True (prime 7),
      testCase "should return false for non prime" $ assertEqual [] False (prime 10)
    ]

partTests :: TestTree
partTests =
  testGroup
    "partTests"
    [ testCase "should return false for 0" $ assertEqual [] 2421 (doPart "part1" testNumbers),
      testCase "should return false for 0" $ assertEqual [] 335 (doPart "part2" testNumbers)
    ]
