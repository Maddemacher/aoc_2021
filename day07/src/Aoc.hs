module Aoc where

import Data.IntMap (fromListWith, toList)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)

costP1 :: Int -> Int -> Int
costP1 pos curr = abs (pos - curr)

costP2 :: Int -> Int -> Int
costP2 pos curr = sum [1 .. (abs (pos - curr))]

findFuleCost :: Int -> [Int] -> (Int -> Int -> Int) -> [Int]
findFuleCost curr numbers costFunc = map (`costFunc` curr) numbers

solveOne :: [Int] -> (Int -> Int -> Int) -> Int
solveOne numbers costFunc = do
  let calculatedCost = map (\x -> findFuleCost x numbers costFunc) [0 .. maximum numbers]
  minimum (map sum calculatedCost)

solve :: [Char] -> [Int] -> Int
solve "part1" numbers = solveOne numbers costP1
solve "part2" numbers = solveOne numbers costP2
solve x _ = error "Not a valid part"

parseInput :: String -> [Int]
parseInput input = map read (splitOn "," input)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
