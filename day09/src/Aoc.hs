module Aoc where

import Data.Char (digitToInt)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, mapMaybe)
import System.Environment.MrEnv (envAsString)

get :: Int -> [a] -> Maybe a
get i l = do
  if i >= 0 && i < length l
    then Just (l !! i)
    else Nothing

getValue :: (Int, Int) -> [String] -> Maybe Char
getValue (x, y) heatmap = do
  let row = fromMaybe [] (get y heatmap)
  get x row

getSurroundingPoints :: (Int, Int) -> [String] -> String
getSurroundingPoints (x, y) heatmap =
  catMaybes
    [ getValue (x - 1, y - 1) heatmap,
      getValue (x, y - 1) heatmap,
      getValue (x + 1, y - 1) heatmap,
      getValue (x - 1, y) heatmap,
      getValue (x + 1, y) heatmap,
      getValue (x - 1, y + 1) heatmap,
      getValue (x, y + 1) heatmap,
      getValue (x + 1, y + 1) heatmap
    ]

getLowPoint :: (Int, Int) -> [String] -> Maybe Int
getLowPoint (x, y) heatmap = do
  let point = digitToInt (fromJust (getValue (x, y) heatmap))
  let surrounding = map digitToInt (getSurroundingPoints (x, y) heatmap)
  if point < minimum surrounding
    then Just point
    else Nothing

getLowPointsForRow :: (String, Int) -> [String] -> [Int]
getLowPointsForRow (row, index) heatmap = do
  let indexedRow = zip row [0 ..]
  mapMaybe (\(c, i) -> getLowPoint (i, index) heatmap) indexedRow

getLowPoints :: [String] -> [Int]
getLowPoints heatmap = do
  let indexed = zip heatmap [0 ..]
  concatMap (`getLowPointsForRow` heatmap) indexed

solve :: [Char] -> [String] -> Int
solve "part1" numbers = sum (map (+ 1) (getLowPoints numbers))
solve "part2" numbers = 10
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (lines input))
