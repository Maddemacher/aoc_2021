module Aoc where

import Data.Function (on)
import Data.List (intersect, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import System.Environment.MrEnv (envAsString)

type ParsedInput = ([String], [String])

addDigit num d = 10 * num + d

fromDigits :: [Int] -> Int
fromDigits = foldl addDigit 0

isZeroOrSixOrNine :: String -> [String] -> Int
isZeroOrSixOrNine input [one, _, four] = do
  if length (intersect one input) == 1
    then 6
    else
      if length (intersect four input) == 4
        then 9
        else 0
isZeroOrSixOrNine _ _ = error "abo"

isTwoOrThreeOrFive :: String -> [String] -> Int
isTwoOrThreeOrFive input [one, _, four] = do
  if length (intersect one input) == 2
    then 3
    else
      if length (intersect four input) == 3
        then 5
        else 2
isTwoOrThreeOrFive _ _ = error "abo"

getDidigt :: [Char] -> Maybe Int
getDidigt input = do
  case length input of
    2 -> Just 1
    3 -> Just 7
    4 -> Just 4
    5 -> Nothing
    6 -> Nothing
    7 -> Just 8
    x -> error "Hej"

getDidigt2 :: [String] -> String -> Int
getDidigt2 signals input = do
  case length input of
    2 -> 1
    3 -> 7
    4 -> 4
    5 -> isTwoOrThreeOrFive input signals
    6 -> isZeroOrSixOrNine input signals
    7 -> 8
    x -> error "Hej"

getDidgits :: ParsedInput -> Int
getDidgits (signals, inputs) = fromDigits (map (getDidigt2 signals) inputs)

getDidgitsFor :: [ParsedInput] -> [Int]
getDidgitsFor = map getDidgits

parseInput1 :: String -> [String]
parseInput1 input = do
  let rows = lines input
  let columns = map (splitOn " | ") rows
  let hej = map last columns
  concatMap words hej

parseRow row = do
  let [a, b] = splitOn " | " row
  (take 3 (sortBy (compare `on` length) (words a)), words b)

parseInput2 :: String -> [ParsedInput]
parseInput2 input = do
  let rows = lines input
  map parseRow rows

solve :: [Char] -> String -> Int
solve "part1" numbers = length (filter isJust (map getDidigt (parseInput1 numbers)))
solve "part2" numbers = sum (getDidgitsFor (parseInput2 numbers))
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part input)
