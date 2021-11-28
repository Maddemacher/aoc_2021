module Day01 where

import Data.List ()
import Data.List.Split (splitOn)
import System.Environment ()
import System.Environment.MrEnv (envAsString)

divisors :: Integral a => a -> [a]
divisors 0 = [1]
divisors 1 = [1]
divisors n = [x | x <- [1 .. n], mod n x == 0]

prime :: Integral a => a -> Bool
prime n = divisors n == [1, n]

notPrime :: Integral a => a -> Bool
notPrime n = not (prime n)

getRelevantNumbers :: Integral a => (a -> Bool) -> [a] -> [(a, a)]
getRelevantNumbers predicate numbers = [(x, i) | (x, i) <- zip numbers [0 ..], predicate x]

partOneAggregate :: Integral a => (a, a) -> a
partOneAggregate (number, index) = number * index

partTwoAggregate :: Integral a => (a, a) -> a
partTwoAggregate (number, index) = if even index then number else - number

doPart :: Integral a => String -> [a] -> a
doPart "part1" numbers = sum (map partOneAggregate (getRelevantNumbers prime numbers))
doPart "part2" numbers = sum (map partTwoAggregate (getRelevantNumbers notPrime numbers))
doPart x input = error "Not a valid part"

main :: IO ()
main = do
  part <- envAsString "part" ""
  input <- readFile "data/input.txt"
  let numbers = map read (splitOn "\n" input)
  print (doPart part numbers)
