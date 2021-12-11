module Aoc where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (nub, sort, sortBy, tails)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Debug.Trace (trace)
import System.Environment.MrEnv (envAsString)

type Point = (Int, Int)

isClosing :: Char -> Char -> Bool
isClosing '(' next = next == ')'
isClosing '[' next = next == ']'
isClosing '{' next = next == '}'
isClosing '<' next = next == '>'
isClosing x _ = False

getCloser :: Char -> Char
getCloser '(' = ')'
getCloser '[' = ']'
getCloser '{' = '}'
getCloser '<' = '>'
getCloser x = error ("Not any closer " ++ [x])

isCloser :: Char -> Bool
isCloser curr = curr `elem` ")]}>"

isOpener :: Char -> Bool
isOpener curr = curr `elem` "([{<"

traverseLine :: [Char] -> [Char] -> Maybe Char
traverseLine "" "" = Nothing
traverseLine stack "" = Just (head stack)
traverseLine stack line
  | isOpener (head line) = traverseLine (head line : stack) (tail line)
  | null stack = Just (head line)
  | isClosing (head stack) (head line) = traverseLine (tail stack) (tail line)
  | isCloser (head line) = Just (head line)
  | otherwise = error "otherwise"

traverseLine2 :: [Char] -> [Char] -> [Char]
traverseLine2 "" "" = []
traverseLine2 stack "" = stack
traverseLine2 stack line
  | isOpener (head line) = traverseLine2 (head line : stack) (tail line)
  | null stack = []
  | isClosing (head stack) (head line) = traverseLine2 (tail stack) (tail line)
  | isCloser (head line) = []
  | otherwise = error "otherwise"

getPoints :: Char -> Int
getPoints ')' = 3
getPoints ']' = 57
getPoints '}' = 1197
getPoints '>' = 25137
getPoints x = 0

getPoints3 :: Char -> Int
getPoints3 ')' = 1
getPoints3 ']' = 2
getPoints3 '}' = 3
getPoints3 '>' = 4
getPoints3 x = 0

getPoints2 :: [Char] -> Int
getPoints2 chars = foldl (\acc curr -> acc * 5 + (getPoints3 curr)) 0 chars

asd numbers = map getCloser (traverseLine2 "" numbers)

asd2 :: [[Char]] -> [[Char]]
asd2 numbers = map asd numbers

asd3 numbers = do
  let nums = sort (filter (> 0) (map getPoints2 (asd2 numbers)))
  nums !! (div (length nums) 2)

solve :: [Char] -> [String] -> Int
solve "part1" numbers = sum (map getPoints (catMaybes (map (\x -> traverseLine "" x) numbers)))
solve "part2" numbers = asd3 numbers
solve x _ = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" "part2"
  input <- readFile "data/input.txt"
  print (solve part (lines input))
