module Aoc where

import Data.Char (digitToInt)
import Data.List (find, group, groupBy, sort, sortOn)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Debug.Trace (trace)
import System.Environment.MrEnv (envAsString)

type Instruction = (String, String)

applyInstructions :: String -> [Instruction] -> String
applyInstructions [x] _ = [x]
applyInstructions row instructions = do
  let curr = take 2 row
  let instruction = find (\(i, _) -> i == curr) instructions
  case instruction of
    Just instruction -> head row : snd instruction ++ applyInstructions (tail row) instructions
    Nothing -> "No instruction found"

getGeneration :: String -> [Instruction] -> Int -> String
getGeneration row instructions generation = foldl (\r _ -> applyInstructions r instructions) row [0 .. (generation - 1)]

getPolymerValue :: [Char] -> [Instruction] -> Int -> Int
getPolymerValue row instructions generation = do
  let groups = group (sort (getGeneration row instructions generation))
  let sorted = sortOn length groups
  length (last sorted) - length (head sorted)

solve :: String -> (String, [Instruction]) -> Int
solve "part1" (row, instructions) = getPolymerValue row instructions 10
solve "part2" (row, instructions) = 0
solve x _ = error "Not a valid part"

parseInstruction :: String -> Instruction
parseInstruction raw = do
  let [combination, insert] = splitOn " -> " raw
  (combination, insert)

parseInput :: String -> (String, [Instruction])
parseInput input = do
  let [row, rawInstructions] = splitOn "\n\n" input
  let instructions = map parseInstruction (lines rawInstructions)
  (row, instructions)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
