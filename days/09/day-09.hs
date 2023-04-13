module Main where

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
type Loc = (Row, Col)

type Visited =  Map Loc Bool

type MoveDir = Char
type MoveAmt = Int

type MoveInstruction = (MoveDir, MoveAmt)


parseInput :: String -> [MoveInstruction]
parseInput fileInput =
  let l = lines fileInput
      parseLine :: String -> MoveInstruction
      parseLine (mvDir:mvAmtStr) =
        (mvDir, read mvAmtStr :: Int)
  in
     map (parseLine) l

updateLoc :: MoveDir -> Loc -> Loc
updateLoc mvDir (r, c) =
  case mvDir of
    'U' -> (r,   c-1)
    'D' -> (r,   c+1)
    'L' -> (r-1, c)
    'R' -> (r+1, c)
    otherwise -> error "Invalid Move. Not in ['U','D','L','R']"

{- 
    * Move the head
    * Tail follows
        - is the FIRST step per move instruction,
          the ONLY time Tail might not move?
        - ex: H moves L then R and now H & T have the same Loc
            ie: T should not move
        - ex: H & T are at the same Loc, H moves
            - T should not move as it is within one unit of H

  =================================================================

    * Following above insight:
        - Test for "must move" is only necessary on FIRST step
        - Subsequent steps will mimic Head's movement.
        - change doOneStep to doFirstStep
        - convert remaining steps to a List, then Map.fromList
        - calculate destination of Head and Tail

  =================================================================

    * Need to check the above against the rules for dealing with
        the situation when Head and Tail are diagonally apart

    * It's slightly different than I assumed:

        "Otherwise, if the head and tail aren't touching and aren't
         in the same row or column, the tail always moves one step
         diagonally to keep up:

              .....    .....    .....
              .....    ..H..    ..H..
              ..H.. -> ..... -> ..T..
              .T...    .T...    .....
              .....    .....    .....

              .....    .....    .....
              .....    .....    .....
              ..H.. -> ...H. -> ..TH.
              .T...    .T...    .....
              .....    .....    ....."

    * So, it's OK for H & T to be diagonally apart by 1 unit in both dimensions
    * But they cannot be apart by more than 1 unit in any direction.
    * Now it looks like T just goes to where H was previously IF it mustMove
-}

doOneStep :: MoveInstruction -> Loc -> Loc -> Visited -> Visited
doOneStep (mvDir, mvAmt) hLoc tLoc visited = visited

main :: IO ()
main = do
  f <- readFile "input-09.txt"

  let moves = parseInput f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ map show $ take 5 $ moves
  putStrLn $ replicate 42 '-'

  let (hLoc:tLoc:startLoc:_) = repeat (0,0)

  putStrLn $ show $ doOneStep (head moves) hLoc tLoc $ Map.fromList [((0,0),True)]
  -- putStrLn $ doMove (head moves) hLoc tLoc $ Map.fromList [((0,0),True)]
