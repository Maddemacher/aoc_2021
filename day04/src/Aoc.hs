module Aoc where

import Control.Monad (forM_, msum)
import Data.Char (digitToInt)
import Data.List (find, inits, intercalate, maximumBy, minimumBy, tails, transpose)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Environment.MrEnv (envAsString)

type Board = [[Int]]

sublist :: Eq a => [a] -> [a] -> Bool
sublist sub universe = all (`elem` universe) sub

compareSolutions :: (Foldable t1, Foldable t2) => (t1 a1, b1) -> (t2 a2, b2) -> Ordering
compareSolutions a b = compare (length (fst a)) (length (fst b))

hasWinningRow :: [Int] -> Board -> Bool
hasWinningRow numbers board = do
  let a = any (`sublist` numbers) board
  let b = any (`sublist` numbers) (transpose board)
  a || b

getScore :: (Num a, Foldable t, Eq a) => ([a], t [a]) -> a
getScore (numbers, board) = do
  let unmarked = [x | x <- concat board, x `notElem` numbers]
  sum unmarked * last numbers

scoreBoard :: [Int] -> [[Int]] -> ([Int], [[Int]])
scoreBoard numbers board = do
  let suggestions = inits numbers
  let solution = find (`hasWinningRow` board) suggestions
  case solution of
    Just solution -> (solution, board)
    Nothing -> error "no solution"

scoreBoards :: [Int] -> [[[Int]]] -> [([Int], [[Int]])]
scoreBoards numbers = map (scoreBoard numbers)

solve :: [Char] -> [Int] -> [[[Int]]] -> Int
solve "part1" numbers boards = getScore (minimumBy compareSolutions (scoreBoards numbers boards))
solve "part2" numbers boards = getScore (maximumBy compareSolutions (scoreBoards numbers boards))
solve x _ _ = error "Not a valid part"

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
  print (solve part numbers boards)
