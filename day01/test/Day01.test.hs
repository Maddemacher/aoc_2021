import Day01 (prime)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Unit tests"
    [doublingMakesNumbersBigger, halvingMakesNumbersSmaller]

doublingMakesNumbersBigger :: TestTree
doublingMakesNumbersBigger =
  testCase "13 is prime" $ assertEqual [] True (prime 13)

halvingMakesNumbersSmaller :: TestTree
halvingMakesNumbersSmaller =
  testCase "9 is not prime" $ assertEqual [] False (prime 9)
