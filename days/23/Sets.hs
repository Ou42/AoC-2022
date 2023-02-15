{-# LANGUAGE BangPatterns #-}

module Sets ( notUsingSetPartA
            , usingSetPartA, usingHashSetPartA
            , usingSetPartB, usingHashSetPartB ) where

import Data.Bifunctor (bimap)
import qualified Data.HashSet as HashSet
import qualified Data.IntSet as IntSet
import qualified Data.Set as S
import Debug.Trace
import System.CPUTime

{-
    Day 23

      Part A
        . Conway's Game of Life type deal
        . Follow rules to move elves
        . Run rules 10x
        . Calc rectangular area fitting
          all elves

      Part B
        . Run rules until no movement
        . How many rounds is that?
-}

type ElfPos = (Int, Int)

elfPosFromFile :: String -> [ElfPos]
elfPosFromFile f =
  concatMap (\(es, y) -> map (\(_,x) -> (x,y)) es)
  . filter (not . null . fst)
  . flip zip [0..]
  . map ((filter ((=='#') . fst))
        . flip zip [0..])
        $ lines f

elfPosFromFileTo2 :: String -> ([ElfPos] -> t) -> t
elfPosFromFileTo2 file fromList =
  -- fromList == S.fromList, HashSet.fromList, id(?), IntSet.fromList(?) ... 
  fromList $ elfPosFromFile file

data Moves =   N  | S  | E  | W
             | NW | NE | SW | SE
             | NN | SS | EE | WW deriving Show

moveToTile :: Moves -> (ElfPos -> ElfPos)
moveToTile N  = \(x,y) -> (x,y-1)
moveToTile S  = \(x,y) -> (x,y+1)
moveToTile E  = \(x,y) -> (x+1,y)
moveToTile W  = \(x,y) -> (x-1,y)
moveToTile NE = moveToTile N . moveToTile E
moveToTile NW = moveToTile N . moveToTile W
moveToTile SE = moveToTile S . moveToTile E
moveToTile SW = moveToTile S . moveToTile W
moveToTile NN = moveToTile N . moveToTile N
moveToTile SS = moveToTile S . moveToTile S
moveToTile EE = moveToTile E . moveToTile E
moveToTile WW = moveToTile W . moveToTile W

allMoves :: ([Bool] -> Bool,[Moves])
allMoves = ((any (==True)), [N, S, W, E, NW, NE, SW, SE])

legalMoves :: Moves -> ([Bool] -> Bool,[Moves])
legalMoves N = ((all (==False)), [N, NE, NW])
legalMoves S = ((all (==False)), [S, SE, SW])
legalMoves E = ((all (==False)), [E, NE, SE])
legalMoves W = ((all (==False)), [W, SW, NW])

initialMoveOrder :: [Moves]
initialMoveOrder = [N, S, W, E]

rotMoveOrder :: [Moves] -> [Moves]
rotMoveOrder (h:t) = t <> [h]

canMoveUsingX :: ElfPos -> ([Bool] -> Bool, [Moves]) -> t -> (ElfPos -> t -> Bool) -> Bool
canMoveUsingX elfPos (mvChk, moves) allPos member =
  mvChk $ map ( (flip member allPos) . (flip moveToTile elfPos) ) moves

-- using a Set or HashSet or ??
--   calc potential move, then check to see if NN, SS, EE, or WW moved there too.
doRndV02UsingX (moveOrder, allPosSet) foldr insert empty member =
  -- create new "Set" from old "Set"
  (rotMoveOrder moveOrder
  -- [✔] - cycle over all elfPos
  , foldr ( \elfPos newSet ->
                  if (canMoveUsingX elfPos allMoves allPosSet member)
                    && (not (isDestDupeX elfPos moveOrder allPosSet))
                    then -- [✔] - store new elfPos
                        insert (getNewElfPosX elfPos moveOrder allPosSet member) newSet
                    else -- [✔] - canMove == False OR Destination is duplicated
                        -- [✔] - store (old) elfPos
                        insert elfPos newSet
          ) empty allPosSet )
  where
    -- [✔] - check if NN, SS, EE, or WW move to the same tile as the current elfPos (here: ePos)
    isDestDupeX ePos moveOrder allPosSet =
      (getNewElfPosX ePos moveOrder allPosSet member)
      `elem` (map (\ePos' -> getNewElfPosX ePos' moveOrder allPosSet member) $ twoAwayX ePos allPosSet)
      where
        -- [✔] - get NN, SS, EE, and WW and check if (a) and elf is standing there and (b) can move
        twoAwayX ePos allPosSet =
          filter (\ePos' -> (member ePos' allPosSet) && (canMoveUsingX ePos' allMoves allPosSet member))
          $ map (flip moveToTile ePos) [NN, SS, EE, WW]

    -- it IS possible that an Elf is *allowed* to move, but cannot!
    getNewElfPosX :: ElfPos -> [Moves] -> t2 -> (ElfPos -> t2 -> Bool) -> ElfPos
    getNewElfPosX ePos moveOrder allPosSet member =
      head $ (<> [ePos]) $ onlyValidMovesX ePos moveOrder allPosSet
      where
        onlyValidMovesX ePos' moveOrder allPosSet =
          Prelude.foldr (\move acc ->
                              if (canMoveUsingX ePos' (legalMoves move) allPosSet member)
                                then ((moveToTile move) ePos') : acc
                                else acc
                        ) [] moveOrder

-- *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

doTenRoundsPartA strFromFile toList fromList foldr insert empty member =
  toList $ snd $ Prelude.foldr (\cnt acc -> doRndV02UsingX acc foldr insert empty member)
                               (initialMoveOrder, allElfPos)
                               [1..10]
    where
      allElfPos = elfPosFromFileTo2 strFromFile fromList

-- partA :: String -> String -> Bool -> (String -> [ElfPos]) -> IO ()
partA strFromFile verTag showTimings tenRndsFunc toList fromList foldr insert empty member = do
  start <- getCPUTime
  let 
      coords = tenRndsFunc strFromFile toList fromList foldr insert empty member

      ((minX, maxX), (minY, maxY)) = bimap minmax minmax $ unzip coords

      area = ((maxX - minX + 1) * (maxY - minY + 1))

  putStrLn $ "------ Part A " <> verTag
  putStrLn $ "minmax X = " <> show minX <> " " <> show maxX
  putStrLn $ "minmax Y = " <> show minY <> " " <> show maxY

  putStr $ show area
  putStrLn " (The minimum orthogonal rectangular area)"

  let numElves = (length coords)
  putStrLn $ "minus " <> show numElves <> " (the # of elves)"
  putStrLn "======"
  putStrLn $ show (area - numElves) <> " (The answer for Part A)"

  end <- getCPUTime

  if showTimings then dispTimings start end
                 else putStr ""
  putStrLn "------------------------"

notUsingSetPartA fileStr timeIt =
  partA fileStr "version 1.x using List:" True doTenRoundsPartA
    id id foldr (:) [] elem

usingSetPartA fileStr timeIt =
  partA fileStr "version 2 using Set:" timeIt doTenRoundsPartA
    S.toList S.fromList S.foldr S.insert S.empty S.member

usingHashSetPartA fileStr timeIt =
  partA fileStr "version 3 using HashSet:" timeIt doTenRoundsPartA
    HashSet.toList HashSet.fromList HashSet.foldr HashSet.insert HashSet.empty HashSet.member

-- *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

doRndsUntilDoneB02UsingSet strFromFile = go 0 (initialMoveOrder, allElfPos) S.empty
  where
    go rnd res@(mo, ap) prevPos =
      if rnd == 2000 || (ap == prevPos)
        then rnd -- (rnd, res)
        else go (rnd+1) (doRndV02UsingX res S.foldr S.insert S.empty S.member) ap
    allElfPos = elfPosFromFileTo2 strFromFile S.fromList

doRndsUntilDoneB02UsingHashSet strFromFile = go 0 (initialMoveOrder, allElfPos) HashSet.empty
  where
    go rnd res@(mo, ap) prevPos =
      if rnd == 2000 || (ap == prevPos)
        then rnd -- (rnd, res)
        else go (rnd+1) (doRndV02UsingX res HashSet.foldr HashSet.insert HashSet.empty HashSet.member) ap
    allElfPos = elfPosFromFileTo2 strFromFile HashSet.fromList

-- partB :: String -> String -> Bool -> (String -> () -> Int) -> IO ()
usingSetPartB strFromFile showTimings = do
  start <- getCPUTime
  let numRounds = doRndsUntilDoneB02UsingSet strFromFile
  putStrLn $ "------ Part B version 2 using Set:"
  putStrLn $ "Rounds until no movement = " <> show numRounds
  end <- getCPUTime

  if showTimings then dispTimings start end
                 else putStr ""
  putStrLn "------------------------"

usingHashSetPartB strFromFile showTimings = do
  start <- getCPUTime
  let numRounds = doRndsUntilDoneB02UsingHashSet strFromFile
  putStrLn $ "------ Part B version 3 using HashSet:"
  putStrLn $ "Rounds until no movement = " <> show numRounds
  end <- getCPUTime

  if showTimings then dispTimings start end
                 else putStr ""
  putStrLn "------------------------"

-- *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

minmax :: [Int] -> (Int, Int)
minmax [x] = (x,x)
minmax (x:xs) = 
  foldr (\n (mn, mx) -> (min n mn, max n mx))
  (x, x)
  xs

dispTimings start end = do
  putStrLn $ "Start = " <> show start <> " end = " <> show end <> " Time = " <> show (end-start)
  putStrLn $ "\t ... or " <> show ( fromIntegral (end-start)  /10^9 ) <> " ms"


main = do
  -- fileStr <- readFile "input-23-test.txt"
  fileStr <- readFile "input-23.txt"

  notUsingSetPartA fileStr True

  usingSetPartA fileStr True

  usingHashSetPartA fileStr True

  -- partB f "version 1:" True doRndsUntilDoneB01

  usingSetPartB fileStr True

  usingHashSetPartB fileStr True
