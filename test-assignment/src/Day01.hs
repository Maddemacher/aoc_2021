module Day01 where

import Data.List ()
import Data.List.Split (splitOn)
import System.Environment ()
import System.Environment.MrEnv (envAsString)

divisors :: Integral a => a -> [a]
divisors 0 = [1]
divisors n = [x | x <- [1 .. n], mod n x == 0]

prime :: Integral a => a -> Bool
prime n = divisors n == [1, n]

getRelevantNumbers :: Integral a => (a -> Bool) -> [a] -> [(a, a)]
getRelevantNumbers predicate numbers = [(x, i) | (x, i) <- zip numbers [0 ..], predicate x]

getAggregator :: Integral a => String -> ((a, a) -> a)
getAggregator "part1" = uncurry (*)
getAggregator "part2" = \(n, i) -> if even i then n else - n
getAggregator x = error "Not a valid part"

getPredicate :: Integral a => String -> (a -> Bool)
getPredicate "part1" = prime
getPredicate "part2" = not . prime
getPredicate x = error "Not a valid part"

solve :: Integral a => String -> [a] -> a
solve part numbers = do
  let aggregator = getAggregator part
  let predicate = getPredicate part
  sum (map aggregator (getRelevantNumbers predicate numbers))

main :: IO ()
main = do
  part <- envAsString "part" ""
  input <- readFile "data/input.txt"
  let numbers = map read (splitOn "\n" input)
  print (solve part numbers)
