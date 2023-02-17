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
-}

main = do
  f <- readFile "input-04.txt"

  let pairs = map (splitOn ",") $ lines f
  -- let pairs = map (break (== ',')) (lines f)

  putStrLn $ show pairs
