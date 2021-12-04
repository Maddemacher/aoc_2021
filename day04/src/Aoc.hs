module Aoc where

import Data.List (find, inits, maximumBy, minimumBy, transpose)
import Data.List.Split (splitOn)
import Data.Ord (comparing)
import System.Environment.MrEnv (envAsString)

type Board = [[Int]]

sublist :: Eq a => [a] -> [a] -> Bool
sublist universe = all (`elem` universe)

compareSolutions :: ([Int], b) -> ([Int], b) -> Ordering
compareSolutions (a, _) (b, _) = compare (length a) (length b)

hasWinningRow :: Board -> [Int] -> Bool
hasWinningRow board numbers = do
  let a = any (sublist numbers) board
  let b = any (sublist numbers) (transpose board)
  a || b

getScore :: (Num a, Foldable t, Eq a) => ([a], t [a]) -> a
getScore (numbers, board) = do
  let unmarked = [x | x <- concat board, x `notElem` numbers]
  sum unmarked * last numbers

scoreBoard :: [Int] -> Board -> ([Int], Board)
scoreBoard numbers board = do
  let solution = find (hasWinningRow board) (inits numbers)
  case solution of
    Just solution -> (solution, board)
    Nothing -> error "no solution"

scoreBoards :: [Int] -> [Board] -> [([Int], Board)]
scoreBoards numbers = map (scoreBoard numbers)

solve :: [Char] -> ([Int], [Board]) -> Int
solve "part1" (numbers, boards) = getScore (minimumBy compareSolutions (scoreBoards numbers boards))
solve "part2" (numbers, boards) = getScore (maximumBy compareSolutions (scoreBoards numbers boards))
solve x _ = error "Not a valid part"

constructBoard :: String -> Board
constructBoard input = do
  let rows = lines input
  let worded = map words rows
  map (map read) worded

parseInput :: String -> ([Int], [Board])
parseInput input = do
  let n : bs = splitOn "\n\n" input
  let numbers = splitOn "," n
  (map read numbers, map constructBoard bs)

main :: IO ()
main = do
  part <- envAsString "part" "part1"
  input <- readFile "data/input.txt"
  print (solve part (parseInput input))
