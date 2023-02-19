module Main where

import Data.List.Split (splitOn)

{-
    NOTE:
      . Running with stack ghci v9.2.5
      . had to set resolver ...
      . this didn't work correctly and had to edit stack.yaml
        . resolver:
            compiler: ghc-9.2.5
      . stack install split
    
    TO RUN:
      . stack ghci --package split
      . :l day-04.hs
      . main

    Day 04

      Part A
        . given a list of 2 ranges (assignment pairs)
        . In how many assignment pairs does one range
          fully contain the other?
      
      Part B
        . Instead, the Elves would like to know the
          number of pairs that overlap at all.
-}

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

toIntList :: [String] -> [Int]
toIntList = map read

type Rng = (Int, Int)

isLow2High :: (Rng, Rng) -> Bool
isLow2High ((r1a, r1b), (r2a, r2b)) = r1a <= r1b && r2a <= r2b

isContained :: (Rng, Rng) -> Bool
isContained ((r1a, r1b), (r2a, r2b)) =
  (r1a >= r2a && r1b <= r2b)
  ||
  (r2a >= r1a && r2b <= r1b)

overlaps :: (Rng, Rng) -> Bool
overlaps ((r1a, r1b), (r2a, r2b)) =
  (r1a >= r2a && r1a <= r2b)
  ||
  (r1b >= r2a && r1b <= r2b)
  ||
  (r2a >= r1a && r2a <= r1b)
  ||
  (r2b >= r1a && r2b <= r1b)

main = do
  f <- readFile "input-04.txt"

  -- convert file containing lines in XX-XX,XX-XX format where X's are numbers
  -- to a [(Rng, Rng)] where Rng == (Int, Int)
  let pairs = map (tuplify2 . map (tuplify2 . toIntList . splitOn ("-")) . splitOn ",") $ lines f

  -- putStrLn $ show pairs
  -- putStrLn $ show $ length pairs

  putStr "Are all the Ranges in ascending order? "
  putStrLn $ show $ 0 == (length $ filter (not . isLow2High) pairs)

  putStrLn "Part A:"
  putStrLn "\tIn how many assignment pairs does one range"
  putStr "\t...fully contain the other? "
  putStrLn $ show $ length $ filter isContained pairs

  putStrLn ""

  putStrLn "Part B:"
  putStr "\tIn how many assignment pairs do the ranges overlap? "
  putStrLn $ show $ length $ filter overlaps pairs
