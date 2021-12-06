module Aoc where

import Data.IntMap (fromListWith, toList)
import Data.List (dropWhileEnd, group, sort)
import Data.List.Split (splitOn, splitWhen)
import System.Environment.MrEnv (envAsString)

frequency :: [Int] -> [(Int, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

simulate :: [Int] -> Int -> [Int]
simulate [zeros, ones, twos, threes, fours, fives, sixs, sevens, eights] iteration =
  [ ones,
    twos,
    threes,
    fours,
    fives,
    sixs,
    sevens + zeros,
    eights,
    zeros
  ]
simulate x _ = error ("Too many" ++ show x)

simulateIterations :: [Int] -> Int -> [Int]
simulateIterations numbers iterations = do
  let r = [0 .. iterations -1]
  foldl simulate numbers r

solveOne :: [Int] -> Int
solveOne numbers = sum (simulateIterations numbers 80)

solveTwo :: [Int] -> Int
solveTwo numbers = sum (simulateIterations numbers 256)

solve :: [Char] -> [Int] -> Int
solve "part1" numbers = solveOne numbers
solve "part2" numbers = solveTwo numbers
solve x _ = error "Not a valid part"

parseInput :: String -> [Int]
parseInput input = do
  let numbers = map read (splitOn "," input)
  map (subtract 1 . snd) (frequency ([0 .. 8] ++ numbers))

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
