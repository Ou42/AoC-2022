module Main where

import Data.Char (isAlpha)
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

data From = From Col deriving (Show)
data To = To Col deriving (Show)

data Move = Move Qty From To deriving (Show)

parseMove :: String -> Move
parseMove moveStr =
  let [qty, fromTo] = splitOn " from " $ concat $ splitOn "move " moveStr
      [from, to] = map toInt $ splitOn " to " fromTo
  in Move (toInt qty) (From from) (To to)

main :: IO ()
main = do
  f <- readFile "input-05.txt"

  -- split file into container stacks and instructions
  let [boxesRaw, movesRaw] = map lines $ splitOn "\n\n" f

  -- extract list of boxes per column into a [["box"]]
  --   note: first "column" is at "index" zero
  let boxesInCols = filter (not . null) $ map (filter isAlpha) $ transpose boxesRaw

  -- convert movesRaw into [Moves]
  let moves = map parseMove movesRaw

  putStrLn "Containers:"
  -- putStrLn $ show boxesRaw
  putStrLn $ unlines boxesRaw
  putStrLn $ show boxesInCols
  putStrLn $ unlines boxesInCols

  putStrLn "Move Instructions:"
  putStrLn $ show $ take 3 movesRaw

  putStrLn ""

  putStrLn $ unlines $ take 3 $ map show moves
