module Aoc where

import Data.List (tails)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)

addMovement :: (Int, Int) -> ([Char], Int) -> (Int, Int)
addMovement (x, y) ("forward", amount) = (x + amount, y)
addMovement (x, y) ("up", amount) = (x, y - amount)
addMovement (x, y) ("down", amount) = (x, y + amount)
addMovement x _ = error "Unknown direction"

addMovementDir :: (Int, Int, Int) -> ([Char], Int) -> (Int, Int, Int)
addMovementDir (x, y, aim) ("forward", amount) = (x + amount, y + (aim * amount), aim)
addMovementDir (x, y, aim) ("up", amount) = (x, y, aim - amount)
addMovementDir (x, y, aim) ("down", amount) = (x, y, aim + amount)
addMovementDir x _ = error "Unknown direction"

solve :: String -> [([Char], Int)] -> Int
solve "part1" movements = uncurry (*) (foldl addMovement (0, 0) movements)
solve "part2" movements = do
  let (x, y, _) = foldl addMovementDir (0, 0, 0) movements
  x * y
solve x _ = error "Not a valid part"

splitMovement :: String -> ([Char], Int)
splitMovement text = do
  let [x, y] = splitOn " " text
  (x, read y)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let numbers = map splitMovement (splitOn "\n" input)
  print (solve part numbers)
