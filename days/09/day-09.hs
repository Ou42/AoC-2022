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
    'U' -> (r+1, c)
    'D' -> (r-1, c)
    'L' -> (r, c-1)
    'R' -> (r, c+1)
    otherwise -> error "Invalid Move. Not in ['U','D','L','R']"

{- 
    * Move the head
    * Tail follows
        - is the FIRST step per move instruction,
          the ONLY time Tail might not move? [NO! NOT TRUE!]
        - ex: H moves L then R and now H & T have the same Loc
            ie: T should not move
        - ex: H & T are at the same Loc, H moves
            - T should not move as it is within one unit of H

  =================================================================

    * Following above insight:
        - [FALSE] Test for "must move" is only necessary on FIRST step [FALSE!]
        - [FALSE] Subsequent steps will mimic Head's movement. [FALSE!]

        [PROOF]
        - ex: - H & T start at same Loc
              - H moves R, T doesn't move
              - H moves U, T doesn't move ( they are now diagonally apart )
              - H moves D 3x, T doesn't move UNTIL 3rd step!

              - cannot easily preprocess the data cancelling out L & R etc
                as the moves might not immediately sequential...
                as in the above example, the last line could be H moves L 3x

        - NEW hypothesis:
              - once T moves, then T will always move until MoveInstr completed

        - convert remaining steps to a List, then Set.fromList
        - calculate destination of Head and Tail

    - changed doOneStep to doFirstStep
    - changed doFirstStep to doStepWithCheck

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

-- check to see if T should follow H
doStepWithCheck :: MoveDir -> MoveRec -> MoveRec
doStepWithCheck mvDir moveRec@(MoveRec {hLoc, tLoc, visited}) =
  let newHLoc = updateLoc mvDir hLoc
  in
     if mustMove newHLoc tLoc
      then moveRec { hLoc = newHLoc
                   , tLoc = hLoc
                   , visited = Set.insert hLoc visited
                   }
      else moveRec { hLoc = newHLoc }

doFullMove :: MoveInstruction -> MoveRec -> MoveRec
doFullMove (mvDir, mvAmt) moveRec@(MoveRec {hLoc, tLoc, visited}) =
  -- mvAmt will always be >= 1
  let twoSteps = undefined -- doStepWithCheck mvDir moveRec
      newTLocs = scanl (flip updateLoc) tLoc $ replicate (mvAmt-1) mvDir
      newTLoc  = last newTLocs
      -- newHLoc  = undefined
      newHLoc  = foldl (flip updateLoc) hLoc $ replicate mvAmt mvDir
  in
      -- slow way -- check each step:
      foldl (flip doStepWithCheck) moveRec $ replicate mvAmt mvDir
      {-
      error "twoSteps not used?! must start w/ updated hLoc & tLoc ..."
      moveRec { hLoc = newHLoc
              , tLoc = newTLoc
              , visited = Set.union visited $ Set.fromList newTLocs
              }
      -}

doAllMoves :: [MoveInstruction] -> MoveRec -> MoveRec
-- do I need to send in a MoveRec?!
-- could I set this up Point Free? Should I?
doAllMoves moveIntrs moveRec = foldl (flip doFullMove) moveRec moveIntrs


main :: IO ()
main = do
  f <- readFile "input-09.txt"

  let moves = parseInput f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ map show $ take 5 $ moves
  putStrLn $ replicate 42 '-'

  let (hLoc:tLoc:startLoc:_) = repeat (0,0)

  let moveRec = MoveRec hLoc tLoc $ Set.fromList [tLoc]

  putStrLn $ show $ doStepWithCheck (fst $ head moves) moveRec

  let moveRec1 = MoveRec (5,5) (5,5) $ Set.fromList [(5,5)]
  let moveRec2 = MoveRec (5,5) (4,5) $ Set.fromList [(4,5)]
  let moveRec3 = MoveRec (5,5) (4,4) $ Set.fromList [(4,4)]

  putStrLn $ replicate 42 '-'
  putStrLn $ show $ head moves
  putStrLn ""
  putStrLn $ show $ moveRec1
  putStrLn $ " ==> " ++ show (doStepWithCheck (fst $ head moves) moveRec1)
  putStrLn ""
  putStrLn $ show $ moveRec2
  putStrLn $ " ==> " ++ show (doStepWithCheck (fst $ head moves) moveRec2)
  putStrLn ""
  putStrLn $ show $ moveRec3
  putStrLn $ " ==> " ++ show (doStepWithCheck (fst $ head moves) moveRec3)

  -- let moveRec4 = MoveRec (0,0) (1,1) $ Set.fromList [(1,1)]
  -- putStrLn ""
  -- putStrLn $ show $ moveRec4
  -- putStrLn $ " ==> " ++ show (doFullMove ('U', 10) moveRec4)

  putStrLn $ replicate 42 '-'
  putStrLn $ show $ moveRec
  let partA = doAllMoves moves moveRec
  putStrLn $ "Part A ==> " ++ show (Set.size $ visited partA)
  putStrLn $ "  ( should == 6494 )"
