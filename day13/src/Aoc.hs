module Aoc where

import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix, matrix)
import System.Environment.MrEnv (envAsString)

type Point = (Int, Int)

type Fold = (Char, Int)

performFold :: [Point] -> Fold -> [Point]
performFold points (direction, index) =
  if direction == 'y'
    then foldHorizontally points index
    else foldVertically points index

foldVertically :: [Point] -> Int -> [Point]
foldVertically points index = do
  let kept = filter (\(x, y) -> x < index) points
  let toFold = filter (\(x, y) -> x > index) points
  let folded = map (\(x, y) -> (2 * index - x, y)) toFold
  nub (kept ++ folded)

foldHorizontally :: [Point] -> Int -> [Point]
foldHorizontally points index = do
  let kept = filter (\(x, y) -> y < index) points
  let toFold = filter (\(x, y) -> y > index) points
  let folded = map (\(x, y) -> (x, 2 * index - y)) toFold
  nub (kept ++ folded)

solve :: [Char] -> ([Point], [Fold]) -> IO ()
solve "part1" (points, x : xs) = print (length (performFold points x))
solve "part2" (points, folds) = print (toMatrix (foldl performFold points folds))
solve x _ = error "Not a valid part"

toMatrix :: [Point] -> Matrix Char
toMatrix points = do
  let cols = maximum (map fst points) + 1
  let rows = maximum (map snd points) + 1
  matrix rows cols (\(row, col) -> if (col - 1, row - 1) `elem` points then '#' else ' ')

parsePoint :: String -> Point
parsePoint raw = do
  let [x, y] = splitOn "," raw
  (read x, read y)

parseFold :: String -> Fold
parseFold raw = do
  let [pre, index] = splitOn "=" raw
  (last pre, read index)

parseInput :: String -> ([Point], [Fold])
parseInput input = do
  let [rawPoints, rawFolds] = splitOn "\n\n" input
  let points = map parsePoint (lines rawPoints)
  let folds = map parseFold (lines rawFolds)
  (points, folds)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let parsed = parseInput input
  solve part parsed
