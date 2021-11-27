module Day01 where

import Data.List
import Data.List.Split
import System.Environment
import System.Environment.MrEnv

double x = x * 2

half x = x / 2

factors :: Integral a => a -> [a]
factors n = [x | x <- [1 .. n], mod n x == 0]

prime :: Integral a => a -> Bool
prime n = factors n == [1, n]

notPrime :: Integral a => a -> Bool
notPrime n = not (prime n)

getRelevantNumbers :: Integral a => (a -> Bool) -> [a] -> [(a, a)]
getRelevantNumbers predicate numbers =
  [(x, i) | (x, i) <- zip numbers [0 ..], predicate x]

partOneAggregate :: Integral a => (a, a) -> a
partOneAggregate (number, index) = number * index

partOne :: Integral a => [a] -> a
partOne numbers = sum (map partOneAggregate (getRelevantNumbers prime numbers))

partTwoAggregate :: Integral a => (a, a) -> a
partTwoAggregate (number, index) =
  if even index
    then number
    else -number

partTwo :: Integral a => [a] -> a
partTwo numbers =
  sum (map partTwoAggregate (getRelevantNumbers notPrime numbers))

doPart :: Integral a => String -> [a] -> a
doPart "part1" input = partOne input
doPart "part2" input = partTwo input
doPart x input = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" ""
  input <- readFile "data/input.txt"
  let numbers = map read (splitOn "\n" input)
  print (doPart part numbers)
