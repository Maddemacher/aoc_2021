module Aoc where

import Data.Char (digitToInt)
import Data.Graph.AStar (aStar)
import Data.HashSet (HashSet, fromList)
import Data.List (transpose)
import System.Environment.MrEnv (envAsString)

type Point = (Int, Int)

getGoalNode :: [[Int]] -> Point
getGoalNode numbers = do
  let ys = length numbers - 1
  let xs = length (head numbers) - 1
  (xs, ys)

isGoalNode :: [[Int]] -> Point -> Bool
isGoalNode numbers point = point == getGoalNode numbers

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x, y) (x1, y1) = abs (x1 - x) + abs (y1 - y)

getTotalCost :: [[Int]] -> [Point] -> Int
getTotalCost numbers = foldl (addCost numbers) 0

addCost :: [[Int]] -> Int -> Point -> Int
addCost numbers current point = do
  let cost = getDistance numbers point point
  cost + current

getPathCost :: [[Int]] -> Int
getPathCost numbers = do
  let path = aStar (getNextNodes numbers) (getDistance numbers) (\x -> manhattanDistance x (getGoalNode numbers)) (isGoalNode numbers) (0, 0)
  case path of
    Just path -> getTotalCost numbers path
    Nothing -> 0

solve :: String -> [[Int]] -> Int
solve "part1" numbers = getPathCost numbers
solve "part2" numbers = do
  let expandedNumbers = expanNumbers numbers
  getPathCost expandedNumbers
solve x _ = error "Not a valid part"

expanNumbers :: [[Int]] -> [[Int]]
expanNumbers numbers = do
  let expandedxs = map expandRow numbers
  let a = transpose expandedxs
  let b = transpose (map expandRow a)
  b

expandRow :: [Int] -> [Int]
expandRow row = do
  let r2 = map (\x -> if (x + 1) > 9 then 1 else x + 1) row
  let r3 = map (\x -> if (x + 2) > 9 then x + 2 - 9 else x + 2) row
  let r4 = map (\x -> if (x + 3) > 9 then x + 3 - 9 else x + 3) row
  let r5 = map (\x -> if (x + 4) > 9 then x + 4 - 9 else x + 4) row
  concat [row, r2, r3, r4, r5]

getNextNodes :: [[Int]] -> Point -> HashSet Point
getNextNodes numbers (x, y) = do
  let raw = [(x, y -1), (x -1, y), (x + 1, y), (x, y + 1)]
  let filtered = filter (\(x, y) -> x >= 0 && x < length (head numbers) && y >= 0 && y < length numbers) raw
  fromList filtered

getDistance :: [[Int]] -> Point -> Point -> Int
getDistance numbers _ (x, y) = (numbers !! y) !! x

parseInput :: String -> [[Int]]
parseInput input = map (map digitToInt) (lines input)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
