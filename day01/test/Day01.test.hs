import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Day01 (double, half)

main = defaultMain unitTests

unitTests =
  testGroup
    "Unit tests"
    [doublingMakesNumbersBigger, halvingMakesNumbersSmaller]

doublingMakesNumbersBigger =
  testCase "Double of 4 is 8" $ assertEqual [] 8 (double 4)

halvingMakesNumbersSmaller =
  testCase "Half of 9 is 4.5" $ assertEqual [] 4.5 (half 9)
