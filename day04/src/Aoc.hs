module Aoc where

import Control.Monad (forM_, msum)
import Data.Char (digitToInt)
import Data.List (find, inits, intercalate, tails, transpose)
import Data.List.Split (splitOn)
import System.Environment.MrEnv (envAsString)

type Board = [[Int]]

sublist :: Eq a => [a] -> [a] -> Bool
sublist sub universe = all (`elem` universe) sub

hasWinningRow :: [Int] -> Board -> Bool
hasWinningRow numbers board = do
  let a = any (`sublist` numbers) board
  let b = any (`sublist` numbers) (transpose board)
  a || b

getPossibleWinner :: [Int] -> [Board] -> Maybe ([Int], Board)
getPossibleWinner numbers boards = do
  let winner = find (hasWinningRow numbers) boards
  case winner of
    Just winner -> Just (numbers, winner)
    Nothing -> Nothing

getWinner :: [Int] -> [Board] -> ([Int], Board)
getWinner numbers boards = do
  let suggestions = inits numbers
  let winner = msum (map (`getPossibleWinner` boards) suggestions)
  case winner of
    Just winner -> winner
    Nothing -> error "No winner"

findWinningBoard numbers boards = do
  let (row, board) = getWinner numbers boards
  let unmarked = [x | x <- concat board, notElem x row]
  sum unmarked * last row

solve "part1" input = uncurry findWinningBoard input
solve x _ = error "Not a valid part"

constructBoard :: String -> Board
constructBoard input = do
  let rows = lines input
  map (\row -> map read (words row)) rows

parseInput :: String -> ([Int], [Board])
parseInput input = do
  let groups = splitOn "\n\n" input
  let numbers = map read (splitOn "," (head groups))
  let boards = map constructBoard (tail groups)
  (numbers, boards)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  let (numbers, boards) = parseInput input
  print (solve part (numbers, boards))
