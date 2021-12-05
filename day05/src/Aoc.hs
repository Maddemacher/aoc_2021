module Aoc where

import qualified Data.Char as Char
import Data.List (minimumBy, nub, transpose)
import Data.List.Split (wordsBy)
import Data.Map (fromListWith, toList)
import System.Environment.MrEnv (envAsString)

type Point = (Int, Int)

type Line = (Point, Point)

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

isStraigt :: Line -> Bool
isStraigt ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

toStraightPointLine :: Line -> [Point]
toStraightPointLine ((x1, y1), (x2, y2)) = do
  let xs = [p | p <- zip (repeat x1) [min y1 y2 .. max y1 y2]]
  let ys = [p | p <- zip [(min x1 x2) .. (max x1 x2)] (repeat y1)]
  nub (xs ++ ys)

getPointRange :: Int -> Int -> [Int]
getPointRange c1 c2 = do
  let range = [min c1 c2 .. max c1 c2]
  if c1 < c2 then range else reverse range

toDiagonalPointLine :: Line -> [Point]
toDiagonalPointLine ((x1, y1), (x2, y2)) = [p | p <- zip (getPointRange x1 x2) (getPointRange y1 y2)]

toPointLine :: Line -> [Point]
toPointLine line = if isStraigt line then toStraightPointLine line else toDiagonalPointLine line

toUsedPoints :: [Line] -> [Point]
toUsedPoints = concatMap toPointLine

countDuplicatePoints :: [Line] -> Int
countDuplicatePoints lines = do
  let freq = frequency (toUsedPoints lines)
  length [p | (p, count) <- freq, count > 1]

solve :: [Char] -> [Line] -> Int
solve "part1" lines = countDuplicatePoints [x | x <- lines, isStraigt x]
solve "part2" lines = countDuplicatePoints lines
solve x _ = error "Not a valid part"

parseLine :: String -> Line
parseLine raw = do
  let [x1, y1, x2, y2] = map read (wordsBy (not . Char.isNumber) raw)
  ((x1, y1), (x2, y2))

parseInput :: String -> [Line]
parseInput input = map parseLine (lines input)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
