module Aoc where

import Data.List (tails)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)

move :: (Int, Int) -> ([Char], Int) -> (Int, Int)
move (x, y) ("forward", amount) = (x + amount, y)
move (x, y) ("up", amount) = (x, y - amount)
move (x, y) ("down", amount) = (x, y + amount)
move x _ = error "Unknown direction"

moveWithAim :: (Int, Int, Int) -> ([Char], Int) -> (Int, Int, Int)
moveWithAim (x, y, aim) ("forward", amount) = (x + amount, y + (aim * amount), aim)
moveWithAim (x, y, aim) ("up", amount) = (x, y, aim - amount)
moveWithAim (x, y, aim) ("down", amount) = (x, y, aim + amount)
moveWithAim x _ = error "Unknown direction"

solve :: String -> [([Char], Int)] -> Int
solve "part1" movements = uncurry (*) (foldl move (0, 0) movements)
solve "part2" movements = do
  let (x, y, _) = foldl moveWithAim (0, 0, 0) movements
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
