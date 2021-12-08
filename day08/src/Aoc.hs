module Aoc where

import Data.IntMap (fromListWith, toList)
import Data.List (group, groupBy, sort)
import Data.List.Split (splitOn)
import Data.List.Utils (replace)
import Data.Maybe (isJust)
import Debug.Trace (trace)
import System.Environment.MrEnv (envAsString)

getDidigt :: [Char] -> Maybe Int
getDidigt input = do
  let grouped = groupBy (==) (sort input)
  case length grouped of
    0 -> Nothing
    1 -> Nothing
    2 -> Just 1
    3 -> Just 7
    4 -> Just 4
    5 -> Nothing
    6 -> Nothing
    7 -> Just 8
    x -> error "Hej"

solve :: [Char] -> [String] -> Int
solve "part1" numbers = length (filter isJust (map getDidigt numbers))
solve "part2" numbers = 10
solve x _ = error "Not a valid part"

parseInput :: String -> [String]
parseInput input = do
  let rows = lines input
  let columns = map (splitOn " | ") rows
  let hej = map last columns
  concat (map (\x -> words x) hej)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
