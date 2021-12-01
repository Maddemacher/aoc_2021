module Day01 where

import Data.List (tails)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)
import Text.Read (readMaybe)

maybeBigger :: (Maybe Int, Maybe Int) -> Bool
maybeBigger (Nothing, curr) = False
maybeBigger (prev, curr) = curr > prev

countIncreases :: [Maybe Int] -> Int
countIncreases numbers = length [x | (x, y) <- zip numbers (Nothing : numbers), maybeBigger (y, x)]

splitNumbers :: [Int] -> [[Int]]
splitNumbers numbers = map (take 3) (tails numbers)

generatePairs :: [[Int]] -> [([Int], [Int])]
generatePairs numberGroups = [(x, y) | [x, y] <- map (take 2) (tails numberGroups)]

solve :: String -> [Int] -> Int
solve "part1" numbers = countIncreases (map Just numbers)
solve "part2" numbers = length [x | (x, y) <- generatePairs (splitNumbers numbers), sum y > sum x]
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let numbers = map read (splitOn "\n" input)
  print (solve part numbers)
