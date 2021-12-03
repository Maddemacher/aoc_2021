module Aoc where

import Data.Char (digitToInt)
import Data.List (intercalate, tails)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)

toDec :: [Char] -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

analyze :: [Char] -> (Char, Char)
analyze values = do
  let zeros = length [x | x <- values, x == '0']
  let ones = length [x | x <- values, x == '1']
  if zeros > ones then ('0', '1') else ('1', '0')

at :: Int -> [a] -> a
at index input = input !! index

getAnalysis :: [[Char]] -> Int -> (Char, Char)
getAnalysis input index = analyze (map (at index) input)

solveOne :: [[Char]] -> Int
solveOne input = do
  let l = length (at 0 input)
  let analysis = map (getAnalysis input) [0 .. l -1]
  let gamma = [x | (x, y) <- analysis]
  let epsilon = [y | (x, y) <- analysis]
  toDec gamma * toDec epsilon

solve :: String -> [[Char]] -> Int
solve "part1" input = solveOne input
solve "part2" input = 0
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let numbers = splitOn "\n" input
  print (solve part numbers)
