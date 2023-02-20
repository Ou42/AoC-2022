module Main where

import Data.Char (isAlphaNum)
import Data.List (transpose)
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
      . :l day-05.hs
      . main

    Day 05

      Part A
        . given 9 stacks of containers listed vertically
          and instructions for moving X containers from
          one column to another
        . Although the instruction might include moving
          more than 1 container, containers are moved
          one at a time.
        . After following the instructions, what are the
          top containers from left to right?
      
-}

toInt :: String -> Int
toInt = read

type Qty = Int
type Col = Int

-- isLow2High :: (Rng, Rng) -> Bool
-- isLow2High ((r1a, r1b), (r2a, r2b)) = r1a <= r1b && r2a <= r2b

-- isContained :: (Rng, Rng) -> Bool
-- isContained ((r1a, r1b), (r2a, r2b)) =
--   (r1a >= r2a && r1b <= r2b)
--   ||
--   (r2a >= r1a && r2b <= r1b)

-- overlaps :: (Rng, Rng) -> Bool
-- overlaps ((r1a, r1b), (r2a, r2b)) =
--   (r1a >= r2a && r1a <= r2b)
--   ||
--   (r1b >= r2a && r1b <= r2b)
--   ||
--   (r2a >= r1a && r2a <= r1b)
--   ||
--   (r2b >= r1a && r2b <= r1b)

main :: IO ()
main = do
  f <- readFile "input-05.txt"

  -- split file into container stacks and instructions
  let [boxesRaw, moves] = map lines $ splitOn "\n\n" f

  let boxes = filter (not . null) $ map (filter isAlphaNum) $ transpose boxesRaw

  putStrLn "Containers:"
  -- putStrLn $ show boxesRaw
  putStrLn $ unlines boxesRaw
  putStrLn $ show boxes
  putStrLn $ unlines boxes
  -- putStrLn "Move Instructions:"
  -- putStrLn $ show moves
