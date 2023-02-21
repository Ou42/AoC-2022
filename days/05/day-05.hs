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

              My Containers came in as:
                [Q] [J]                         [H]
                [G] [S] [Q]     [Z]             [P]
                [P] [F] [M]     [F]     [F]     [S]
                [R] [R] [P] [F] [V]     [D]     [L]
                [L] [W] [W] [D] [W] [S] [V]     [G]
                [C] [H] [H] [T] [D] [L] [M] [B] [B]
                [T] [Q] [B] [S] [L] [C] [B] [J] [N]
                [F] [N] [F] [V] [Q] [Z] [Z] [T] [Q]
                 1   2   3   4   5   6   7   8   9

        . Although the instruction might include moving
          more than 1 container, containers are moved
          one at a time.
        . After following the instructions, list the
          top-most container in each col, left to right
      
-}

toInt :: String -> Int
toInt = read

type Box = Char
type Boxes = [Box]
type ContainersInList = [[Box]]

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

applyMove :: ContainersInList -> Move -> ContainersInList
applyMove cnl (Move qty (From from) (To to)) =
  let payload = reverse $ take qty $ cnl !! (from - 1)
      newTo = (++) payload $ cnl !! (to - 1)
      newFrom = drop qty $ cnl !! (from - 1)
      updateBoxes :: Col -> Boxes -> Boxes
      updateBoxes col boxes
        | col == from = newFrom
        | col == to = newTo
        | otherwise = boxes
 
  in map (\col -> updateBoxes col (cnl !! (col - 1))
         ) [1..(length cnl)]

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

  putStrLn "Initial move instructions ..."
  putStrLn $ '\t' : (show $ take 3 movesRaw)

  -- test one move instruction
  putStrLn $ unlines $ take 3 $ map (((:) '\t') . show) moves
  putStrLn "Result after one move:"
  putStrLn $ '\t' : (show $ applyMove boxesInCols (head moves))

  -- Complete all move instructions in order
  putStrLn "Part A (final result after all move ops):"
  putStrLn $ '\t' : (show $ map head $ foldl applyMove boxesInCols moves)
