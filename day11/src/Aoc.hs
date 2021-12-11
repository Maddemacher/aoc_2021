module Aoc where

import Data.Char (digitToInt)
import Data.Matrix (Matrix (ncols, nrows), fromLists, getElem, mapPos, setElem, toList)
import Data.Maybe (catMaybes)
import System.Environment.MrEnv (envAsString)

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

getFlashGeneration :: (Matrix Int, Int) -> (Matrix Int, Int)
getFlashGeneration (current, generation) = do
  let (next, _) = getNextGeneration (current, 0)
  let nonzero = any (/= 0) (toList next)
  if nonzero
    then getFlashGeneration (next, generation + 1)
    else (current, generation + 1)

solve :: String -> Matrix Int -> Int
solve "part1" current = snd (foldr (\a b -> getNextGeneration b) (current, 0) [0 .. 99])
solve "part2" current = snd (getFlashGeneration (current, 0))
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let rows = lines input
  let parsed = map (map digitToInt) rows
  print (solve part (fromLists parsed))
