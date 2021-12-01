module Day01 where

import Data.List ()
import Data.List.Split (splitOn)

maybeBigger :: Integral a => (a, a) -> Bool
-- maybeBigger (Nothing, curr) = False
maybeBigger (prev, curr) = curr > prev

countIncreases :: Integral a => [a] -> Int
countIncreases numbers = length [x | (x, i) <- zip numbers (10000000 : numbers), maybeBigger (i, x)]

main :: IO ()
main = do
  input <- readFile "data/input.txt"
  let numbers = map read (splitOn "\n" input)
  print (countIncreases numbers)
