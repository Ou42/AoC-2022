{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')

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

      Part B
        . Rather than two knots, you now must simulate a rope consisting of ten
          knots. One knot is still the head of the rope and moves according to
          the series of motions. Each knot further down the rope follows the knot
          in front of it using the same rules as before.
        . Instead of H(ead) and T(ail), it is suggested to use H(ead) and [1..9]
        . How many positions does the tail (9) of the rope visit at least once?
-}

-- Part A

type Row = Int
type Col = Int
type Loc = (Row, Col)

-- for Part B
type Pos = (Row, Col)
type KnotPrevPos = Pos
-- data Knot = HeadKnot Pos | TailKnot Pos | PrevKnotPrevPos Pos | NoPrevKnotPos
data Knot = HeadKnot Pos | TailKnot Pos deriving Show

-- rec: 


-- newtype Prev = Prev Pos
-- data Knot = HeadKnot Pos | TailKnot Pos | PrevKnotPrevPos Prev | NoPrevKnotPos

type Visited =  Set Loc

data MoveRec = MoveRec
  { hLoc :: Loc
  , tLoc :: Loc
  , visited :: Visited
  } deriving Show

{-
data MoveRecB = MoveRecB
  { knots :: [Knot]
  , visitedB :: Visited
  } deriving Show
-}

-- recommended:
data MoveRecB = MoveRecB
  { headKnot :: Pos -- Knot
  , tailKnots :: [Pos] -- vs [Knot] -- don't need Sum type
  , visitedB :: Visited
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
     map parseLine lns

updateLoc :: MoveDir -> Loc -> Loc
updateLoc mvDir (r, c) =
  case mvDir of
    'U' -> (r+1, c)
    'D' -> (r-1, c)
    'L' -> (r, c-1)
    'R' -> (r, c+1)
    _ -> error "Invalid Move. Not in ['U','D','L','R']"

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
  -- slow way -- check each step:
  foldl (flip doStepWithCheck) moveRec $ replicate mvAmt mvDir

doAllMoves :: [MoveInstruction] -> MoveRec -> MoveRec
-- do I need to send in a MoveRec?!
-- could I set this up Point Free? Should I?
doAllMoves moveIntrs moveRec = foldl (flip doFullMove) moveRec moveIntrs

-- Part B

-- mustMoveTail :: Knot -> Knot -> Bool
mustMoveTail :: Pos -> Pos -> Bool
mustMoveTail (r1,c1) (r2,c2) = abs (r1-r2) > 1 || abs (c1-c2) > 1


-- -- moveHeadKnot :: MoveDir -> MoveRecB -> MoveRecB
-- moveHeadKnot :: MoveDir -> Pos -> Pos
-- -- moveHeadKnot mvDir moveRecB@(MoveRecB (HeadKnot hLoc:tKnots) _) =
-- -- moveHeadKnot mvDir hKnotPos = updateLoc mvDir hKnotPos
-- moveHeadKnot = updateLoc
--   -- let newHLoc = updateLoc mvDir hLoc
  -- in
  --     moveRecB { knots = HeadKnot newHLoc:tKnots }

-- check to see if the *next* Tail should follow it's predecessor
-- moveTails :: (Knot, KnotPrevPos) -> MoveRecB -> MoveRecB
moveTails :: (Pos, Pos) -> MoveRecB -> MoveRecB
moveTails prevKnotInfo moveRecB = go [] prevKnotInfo moveRecB
  where
    go :: [Pos] -> (Pos, Pos) -> MoveRecB -> MoveRecB
    go knotAccu (tKnotPos, _) moveRecB@(MoveRecB { tailKnots = [], visitedB }) =
      moveRecB { tailKnots = reverse knotAccu
               , visitedB = Set.insert tKnotPos visitedB
               }

    go knotAccu (knotPos, knotPrevPos) moveRecB@(MoveRecB { tailKnots = nextPrevPos:tKnots, visitedB }) =
      if mustMoveTail knotPos nextPrevPos
        then go (knotPrevPos : knotAccu) (knotPrevPos, nextPrevPos) moveRecB { tailKnots = tKnots }
        else -- tKnot2 doesn't move. No more tail moves! Can short-circuit!
            moveRecB { tailKnots = reverse knotAccu ++ nextPrevPos:tKnots }

-- rec:
--   math is much faster
--   but if keeping the prevHPos, just add field to Rec

-- oneMovePartB :: MoveDir -> MoveRecB -> MoveRecB
oneMovePartB :: MoveRecB -> MoveDir -> MoveRecB
oneMovePartB moveRecB@(MoveRecB { headKnot, tailKnots }) mvDir =
  let -- prevHPos = headKnot -- not needed because everything is immutable
      -- newMoveRecB = moveHeadKnot mvDir moveRecB
      -- newHKnot = head $ knots newMoveRecB
      newHKnot = updateLoc mvDir headKnot
      -- newTails = moveTails (newHKnot, headKnot) moveRecB { headKnot  = newHKnot }
  in
      -- newTails { headKnot = newHKnot }
      moveTails (newHKnot, headKnot) moveRecB { headKnot  = newHKnot }

-- doFullMoveB :: MoveInstruction -> MoveRecB -> MoveRecB
doFullMoveB :: MoveRecB -> MoveInstruction -> MoveRecB
doFullMoveB moveRecB (mvDir, mvAmt) =
  -- slow way -- check each step:
  -- foldl' (flip oneMovePartB) moveRecB $ replicate mvAmt mvDir
  foldl' oneMovePartB moveRecB $ replicate mvAmt mvDir

-- doAllMovesB :: [MoveInstruction] -> MoveRecB -> MoveRecB
doAllMovesB :: MoveRecB -> [MoveInstruction] -> MoveRecB
-- do I need to send in a MoveRec?!
-- could I set this up Point Free? Should I?
-- doAllMovesB moveInstrs moveRecB = foldl' (flip doFullMoveB) moveRecB moveInstrs
-- doAllMovesB = foldl' (flip doFullMoveB)
doAllMovesB = foldl' doFullMoveB



main :: IO ()
main = do
  f <- readFile "input-09.txt"

  let moves = parseInput f

  -- putStrLn $ replicate 42 '-'
  -- putStrLn $ unlines $ map show $ take 5 $ moves
  -- putStrLn $ replicate 42 '-'

  let (hLoc:tLoc:startLoc:_) = repeat (0,0)

  let moveRec = MoveRec hLoc tLoc $ Set.fromList [tLoc]

  -- putStrLn $ show $ doStepWithCheck (fst $ head moves) moveRec

  putStrLn $ replicate 42 '-'
  -- putStrLn $ show $ moveRec
  let partA = doAllMoves moves moveRec
  putStrLn $ "Part A ==> " ++ show (Set.size $ visited partA)
  putStrLn   "  ( should == 6494 )"

  putStrLn $ replicate 42 '='
  putStrLn "  -- Part B"
  putStrLn $ replicate 42 '-'

  let headKnot  = (0,0)
  let tailKnots = replicate 9 (0,0)
  let visitedB  = Set.singleton (0,0)

  let moveRecB = MoveRecB { headKnot, tailKnots, visitedB }

  print moveRecB

  print $ oneMovePartB moveRecB 'L'

  -- let moveR10 = head $ drop 9 $ iterate (oneMovePartB 'R') moveRecB
  -- let moveR10 = iterate (flip oneMovePartB 'R') moveRecB !! 10
  let moveR10 = iterate (`oneMovePartB` 'R') moveRecB !! 10

  print moveR10

{-
  ghci> main
  ------------------------------------------
  Part A ==> 6494
    ( should == 6494 )
  ==========================================
    -- Part B
  ------------------------------------------
  MoveRecB {knots = [HeadKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0)], visitedB = fromList [(0,0)]}
  MoveRecB {knots = [HeadKnot (0,-1),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0),TailKnot (0,0)], visitedB = fromList [(0,0)]}
  MoveRecB {knots = [HeadKnot (0,10),TailKnot (0,9),TailKnot (0,8),TailKnot (0,7),TailKnot (0,6),TailKnot (0,5),TailKnot (0,4),TailKnot (0,3),TailKnot (0,2),TailKnot (0,1)], visitedB = fromList [(0,0),(0,1)]}
-}

  let partB@MoveRecB { visitedB = vb } = doAllMovesB moveRecB moves
  putStrLn $ "Part B ==> " ++ show (Set.size vb)

{-
  That's not the right answer; your answer is too high. If you're stuck, make
  sure you're using the full input data; there are also some general tips on
  the about page, or you can ask for hints on the subreddit. Please wait one
  minute before trying again. (You guessed 5143.) [Return to Day 9]
-}

  -- let ks = knots partB

  -- print (tailKnots partB)
  -- print moveRecB


