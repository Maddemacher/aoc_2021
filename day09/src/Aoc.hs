module Aoc where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Debug.Trace (trace)
import System.Environment.MrEnv (envAsString)

debug = flip trace

type Point = (Int, Int)

test =
  lines
    "2199943210\n\
    \3987894921\n\
    \9856789892\n\
    \8767896789\n\
    \9899965678"

tp :: (Point, Int)
tp = ((3, 3), 5)

get :: Int -> [a] -> Maybe a
get i l = do
  if i >= 0 && i < length l
    then Just (l !! i)
    else Nothing

getValue :: Point -> [String] -> Maybe Char
getValue (x, y) heatmap = do
  let row = fromMaybe [] (get y heatmap)
  get x row

getSurroundingPoints :: Point -> [String] -> String
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

getP2Points :: Point -> [String] -> [(Point, Int)]
getP2Points (x, y) heatmap = do
  let positions = [(x, y -1), (x - 1, y), (x + 1, y), (x, y + 1)]
  let asd = map (\x -> (x, getValue x heatmap)) positions
  map (\(x, v) -> (x, maybe 9 digitToInt v)) asd

getLowPoint :: Point -> [String] -> Maybe (Point, Int)
getLowPoint (x, y) heatmap = do
  let value = digitToInt (fromJust (getValue (x, y) heatmap))
  let surrounding = map digitToInt (getSurroundingPoints (x, y) heatmap)
  if value < minimum surrounding
    then Just ((x, y), value)
    else Nothing

getLowPointsForRow :: (String, Int) -> [String] -> [(Point, Int)]
getLowPointsForRow (row, index) heatmap = do
  let indexedRow = zip row [0 ..]
  mapMaybe (\(c, i) -> getLowPoint (i, index) heatmap) indexedRow

getLowPoints :: [String] -> [(Point, Int)]
getLowPoints heatmap = do
  let indexed = zip heatmap [0 ..]
  concatMap (`getLowPointsForRow` heatmap) indexed

isLarger curr lowPoint = snd curr /= 9 && (snd curr > snd lowPoint)

getBasin :: (Point, Int) -> [String] -> [(Point, Int)]
getBasin lowPoint heatmap = do
  let surrounding = getP2Points (fst lowPoint) heatmap
  let larger = filter (`isLarger` lowPoint) surrounding
  let isnull = (null larger)
  if isnull
    then []
    else concat [larger, (concatMap (\x -> getBasin x heatmap) larger)]

getBasins :: [String] -> [[(Point, Int)]]
getBasins heatmap = do
  let lowPoints = getLowPoints heatmap
  (map nub (map (`getBasin` heatmap) lowPoints))

solve :: [Char] -> [String] -> Int
solve "part1" numbers = sum (map (\x -> snd x + 1) (getLowPoints numbers))
solve "part2" numbers = do
  let sorted = sortBy (compare `on` length) (getBasins numbers)
  let [a, b, c] = map (+ 1) (map length (take 3 (reverse sorted)))
  a * b * c
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (lines input))
