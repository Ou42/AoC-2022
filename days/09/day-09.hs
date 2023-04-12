module Main where

-- import Data.List (transpose)
import qualified Data.Map as Map
import Data.Map (Map(..))

{-
    Day 09

      Part A
        . Given a movement instructions L(eft),R(ight),U(p),D(own) & iterations
        . Track the location of a Head knot and the Tail knot following it
        . The Tail must always be within 1 horizontal or vertical step of Head
        . When the Tail is diagonally away from the Head and the Head moves...
              . then the Tail jumps to the same Column or Row
              . If the Head moved U(p) or D(own) then, to the same Column
              . If the head moved R(ight) or L(eft) then, to the same Row
              . But in BOTH cases, still one step away
        . The Head and the Tail both start at the same position, overlapping.

        . After following all movement instructions, how many locations did the
            Tail visit at least once?
-}

-- Part A

type Row = Int
type Col = Int

type Visited =  Map (Row,Col) Bool

type MoveDir = Char
type MoveAmt = Int

data MoveInstruction = MI MoveDir MoveAmt

-- parseInput :: String -> [MoveInstruction]
parseInput fileInput =
  let l = lines fileInput
  in
     map (words) l

main :: IO ()
main = do
  f <- readFile "input-09.txt"

  let moves = lines f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ take 5 $ moves
  putStrLn $ replicate 42 '-'

