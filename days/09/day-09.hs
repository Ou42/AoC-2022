{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as Set

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

type Visited =  Set Loc

data MoveRec = MoveRec
  { hLoc :: Loc
  , tLoc :: Loc
  , visited :: Visited
  } deriving Show

type MoveDir = Char
type MoveAmt = Int

type MoveInstruction = (MoveDir, MoveAmt)


parseInput :: String -> [MoveInstruction]
parseInput fileInput =
  let lns = lines fileInput
      parseLine :: String -> MoveInstruction
      parseLine (mvDir:mvAmtStr) =
        (mvDir, read mvAmtStr :: Int)
  in
     map (parseLine) lns

updateLoc :: MoveDir -> Loc -> Loc
updateLoc mvDir (r, c) =
  case mvDir of
    'U' -> (r-1, c)
    'D' -> (r+1, c)
    'L' -> (r, c-1)
    'R' -> (r, c+1)
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
        - convert remaining steps to a List, then Set.fromList
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

mustMove :: Loc -> Loc -> Bool
mustMove (r1,c1) (r2,c2) = abs (r1-r2) > 1 || abs (c1-c2) > 1

doFirstStep :: MoveInstruction -> MoveRec -> MoveRec
doFirstStep (mvDir, mvAmt) moveRec@(MoveRec {hLoc, tLoc, visited}) =
  let newHLoc = updateLoc mvDir hLoc
  in
     if mustMove newHLoc tLoc
      then moveRec { hLoc = newHLoc
                   , tLoc = hLoc
                   , visited = Set.insert hLoc visited
                   }
      else moveRec { hLoc = newHLoc }

doFullMove :: MoveInstruction -> MoveRec -> MoveRec
doFullMove moveInstr@(mvDir, mvAmt) moveRec@(MoveRec {hLoc, tLoc, visited}) =
  -- mvAmt will always be > 1
  let movesToMake = [1..(mvAmt-1)]
      afterFirst  = doFirstStep moveInstr moveRec
  in
     error "scanl FTW?!"

main :: IO ()
main = do
  f <- readFile "input-09.txt"

  let moves = parseInput f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ map show $ take 5 $ moves
  putStrLn $ replicate 42 '-'

  let (hLoc:tLoc:startLoc:_) = repeat (0,0)

  let moveRec = MoveRec hLoc tLoc $ Set.fromList [tLoc]

  putStrLn $ show $ doFirstStep (head moves) moveRec

  let moveRec1 = MoveRec (5,5) (5,5) $ Set.fromList [(5,5)]
  let moveRec2 = MoveRec (5,5) (4,5) $ Set.fromList [(4,5)]
  let moveRec3 = MoveRec (5,5) (4,4) $ Set.fromList [(4,4)]

  putStrLn $ replicate 42 '-'
  putStrLn $ show $ head moves
  putStrLn ""
  putStrLn $ show $ moveRec1
  putStrLn $ " ==> " ++ show (doFirstStep (head moves) moveRec1)
  putStrLn ""
  putStrLn $ show $ moveRec2
  putStrLn $ " ==> " ++ show (doFirstStep (head moves) moveRec2)
  putStrLn ""
  putStrLn $ show $ moveRec3
  putStrLn $ " ==> " ++ show (doFirstStep (head moves) moveRec3)

  putStrLn $ show (doFullMove ('D',10) (MoveRec (0,0) (1,1) Set.empty))
  