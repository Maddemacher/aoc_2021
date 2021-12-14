module Aoc where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map (), findWithDefault, foldlWithKey, fromList, insert, toList, (!))
import qualified Data.Map.Strict as Map
import System.Environment.MrEnv (envAsString)

solve :: [Char] -> (String, Map String (String, Int)) -> Int
solve "part1" (row, instructions) = getPolymerValue (setupInstructions row instructions) 10
solve "part2" (row, instructions) = getPolymerValue (setupInstructions row instructions) 40
solve x _ = error "Not a valid part"

getPolymerValue :: Map String (String, Int) -> Int -> Int
getPolymerValue template generations = do
  let final = foldl getNextGeneration template [1 .. generations]
  let sorted = sortBy (on compare snd) (toList (foldl toMap Map.empty (toList final)))
  snd (last sorted) - snd (head sorted)

toMap :: Map Char Int -> (String, (String, Int)) -> Map Char Int
toMap m (k, (_, count)) = do
  let key = last k
  let c = findWithDefault 0 key m
  insert key (count + c) m

getNextGeneration :: Map String (String, Int) -> Int -> Map String (String, Int)
getNextGeneration template _ = foldlWithKey updateTemplate template template

updateTemplate :: Map String (String, Int) -> String -> (String, Int) -> Map String (String, Int)
updateTemplate template key (after, count) = do
  let a = head key : after
  let b = after ++ [last key]

  let currA = template ! a
  let afterA = insert a (fst currA, snd currA + count) template

  let currB = afterA ! b
  let afterB = insert b (fst currB, snd currB + count) afterA

  let currC = afterB ! key
  insert key (fst currC, snd currC - count) afterB

setupInstructions :: String -> Map String (String, Int) -> Map String (String, Int)
setupInstructions [x] instructions = instructions
setupInstructions row instructions = do
  let key = take 2 row
  let (after, count) = instructions ! key
  let updated = insert key (after, count + 1) instructions
  setupInstructions (tail row) updated

parseInstruction :: String -> (String, (String, Int))
parseInstruction raw = do
  let [combination, insert] = splitOn " -> " raw
  (combination, (insert, 0))

parseInput :: String -> (String, Map String (String, Int))
parseInput input = do
  let [row, rawInstructions] = splitOn "\n\n" input
  let instructions = map parseInstruction (lines rawInstructions)
  (row, Map.fromList instructions)

main :: IO ()
main = do
  part <- envAsString "part" "part2"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
