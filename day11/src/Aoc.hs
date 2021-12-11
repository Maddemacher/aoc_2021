module Aoc where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (nub, sort, sortBy, tails)
import Data.List.Split (splitOn)
import Data.Matrix (Matrix (ncols, nrows), fromLists, getElem, mapPos, setElem, toList)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Debug.Trace (trace)
import System.Environment.MrEnv (envAsString)

test =
  fromLists
    [ [1, 1, 1, 1, 1],
      [1, 9, 9, 9, 1],
      [1, 9, 1, 9, 1],
      [1, 9, 9, 9, 1],
      [1, 1, 1, 1, 1]
    ]

test2 =
  fromLists
    [ [5, 4, 8, 3, 1, 4, 3, 2, 2, 3],
      [2, 7, 4, 5, 8, 5, 4, 7, 1, 1],
      [5, 2, 6, 4, 5, 5, 6, 1, 7, 3],
      [6, 1, 4, 1, 3, 3, 6, 1, 4, 6],
      [6, 3, 5, 7, 3, 8, 5, 4, 7, 8],
      [4, 1, 6, 7, 5, 2, 4, 6, 4, 5],
      [2, 1, 7, 6, 8, 4, 1, 7, 2, 1],
      [6, 8, 8, 2, 8, 8, 1, 1, 3, 4],
      [4, 8, 4, 6, 8, 4, 8, 5, 5, 4],
      [5, 2, 8, 3, 7, 5, 1, 5, 2, 6]
    ]

type Point = (Int, Int)

getFlashes :: Matrix Int -> [Point]
getFlashes current = do
  catMaybes (toList (mapPos (\p v -> if v > 9 then Just p else Nothing) current))

applyFlashes :: Matrix Int -> Matrix Int
applyFlashes current = do
  let flashes = getFlashes current
  if null flashes
    then current
    else do
      let updated = foldl (flip updateValue) current flashes
      applyFlashes updated

updateValue :: Point -> Matrix Int -> Matrix Int
updateValue (x, y) current = do
  let flashed = setElem 0 (x, y) current
  let positions = [(x -1, y -1), (x, y -1), (x + 1, y -1), (x -1, y), (x + 1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1)]
  foldl (\current (x, y) -> incrementValue (x, y) current) flashed positions

incrementValue :: Point -> Matrix Int -> Matrix Int
incrementValue (x, y) current =
  if x > 0 && y > 0 && x <= ncols current && y <= nrows current
    then do
      let val = getElem x y current
      if val == 0
        then current
        else setElem (val + 1) (x, y) current
    else current

getNextGeneration :: (Matrix Int, Int) -> (Matrix Int, Int)
getNextGeneration (current, count) = do
  let after = mapPos (\(r, c) a -> a + 1) current
  let flashed = applyFlashes after
  let zeros = filter (== 0) (toList flashed)
  (flashed, count + length zeros)

solve :: String -> Matrix Int -> (Matrix Int, Int)
solve "part1" current = foldr (\a b -> getNextGeneration b) (current, 0) [0 .. 99]
-- solve "part2" current =
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let rows = lines input
  let asd = map (\row -> map digitToInt row) rows
  print (solve part (fromLists asd))
