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

doFilterCommon :: Int -> [[Char]] -> [[Char]]
doFilterCommon index input = do
  let (common, _) = getAnalysis input index
  [x | x <- input, at index x == common]

doFilterUncommon :: Int -> [[Char]] -> [[Char]]
doFilterUncommon index input = do
  let (_, uncommon) = getAnalysis input index
  [x | x <- input, at index x == uncommon]

asd :: Int -> [[Char]] -> (Int -> [[Char]] -> [[Char]]) -> [Char]
asd index [x] filterFunc = x
asd index input filterFunc = do
  let hej = filterFunc index input
  asd (index + 1) hej filterFunc

solveOne :: [[Char]] -> Int
solveOne input = do
  let l = length (at 0 input)
  let analysis = map (getAnalysis input) [0 .. l -1]
  let gamma = [x | (x, y) <- analysis]
  let epsilon = [y | (x, y) <- analysis]
  toDec gamma * toDec epsilon

solveTwo :: [[Char]] -> Int
solveTwo input = do
  let oxygen = asd 0 input doFilterCommon
  let scrubber = asd 0 input doFilterUncommon
  toDec oxygen * toDec scrubber

solve :: String -> [[Char]] -> Int
solve "part1" input = solveOne input
solve "part2" input = solveTwo input
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let numbers = splitOn "\n" input
  print (solve part numbers)
