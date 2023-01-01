{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.HashSet as HashSet
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace
import System.CPUTime

import Sets (partA, doTenRoundsPartA)

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
type ElfPosSet = S.Set ElfPos
type ElfPosHashSet = HashSet.HashSet ElfPos

elfPosFromFile :: String -> [ElfPos]
elfPosFromFile f =
  concatMap (\(es, y) -> map (\(_,x) -> (x,y)) es)
  . filter (not . null . fst)
  . flip zip [0..]
  . map ((filter ((=='#') . fst))
        . flip zip [0..])
        $ lines f

elfPosFromFileToSet :: String -> ElfPosSet
elfPosFromFileToSet f =
  S.fromList $ elfPosFromFile f

elfPosFromFileToHashSet :: String -> ElfPosHashSet
elfPosFromFileToHashSet f =
  HashSet.fromList $ elfPosFromFile f

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

canMove :: ElfPos -> ([Bool] -> Bool, [Moves]) -> [ElfPos] -> Bool
canMove elfPos (mvChk, moves) allPos =
  mvChk
  $ map ( (flip elem allPos)
        . (flip moveToTile elfPos)
        ) moves

canMoveUsingSet :: ElfPos -> ([Bool] -> Bool, [Moves]) -> S.Set ElfPos -> Bool
canMoveUsingSet elfPos (mvChk, moves) allPos =
  mvChk
--  $ map ( (flip elem allPos)
  $ map ( (flip S.member allPos)
          . (flip moveToTile elfPos)
          ) moves

canMoveUsingHashSet :: ElfPos -> ([Bool] -> Bool, [Moves]) -> HashSet.HashSet ElfPos -> Bool
canMoveUsingHashSet elfPos (mvChk, moves) allPos =
  mvChk
--  $ map ( (flip elem allPos)
  $ map ( (flip HashSet.member allPos)
          . (flip moveToTile elfPos)
          ) moves

-- create a Map to check if 2+ elves landed on same tile
-- k v == (new pos) [(old pos)]
doRndV01UsingMap (moveOrder, allPos) = 
  -- map creates a List of Maps. foldf creates ONE Map:
  let mapPossPos = foldr ( \ePos eMap -> M.insertWith (<>) (getNewElfPos ePos) [ePos] eMap ) M.empty allPos
                      where
                        getNewElfPos elfPos = (moveOrNot elfPos) $ (canMove elfPos allMoves allPos)
                        moveOrNot elfPos False = elfPos
                        moveOrNot elfPos True  = newElfPos'
                          where
                            -- it IS possible that an Elf is *allowed* to move, but cannot!
                            newElfPos' = head $ (<> [elfPos]) onlyValidMoves
                            onlyValidMoves = foldr (\move acc ->
                                                            if (canMove elfPos (legalMoves move) allPos)
                                                              then ((moveToTile move) elfPos) : acc
                                                              else acc
                                                    ) [] moveOrder

      possPos = M.keys mapPossPos

  in
    (rotMoveOrder moveOrder, concatMap (\(possPos', oldPos) -> if (length oldPos) == 1
                                                                 then [possPos']
                                                                 else oldPos)
                             $ M.toList mapPossPos)

-- using a Set, calc potential move, then check to see if NN, SS, EE, or WW moved there too.
doRndV02UsingSet (moveOrder, allPosSet) =
  -- create new Set from old Set
  (rotMoveOrder moveOrder
  -- [✔] - cycle over all elfPos
  , S.foldr ( \elfPos newSet ->
                  if (canMoveUsingSet elfPos allMoves allPosSet) 
                     && (not (isDestDupe elfPos moveOrder allPosSet))
                    then -- [✔] - store new elfPos
                        S.insert (getNewElfPos elfPos moveOrder allPosSet) newSet
                    else -- [✔] - canMove == False OR Destination is duplicated
                        -- [✔] - store (old) elfPos
                        S.insert elfPos newSet
            ) (S.empty :: S.Set ElfPos) allPosSet )

-- using a HashSet, calc potential move, then check to see if NN, SS, EE, or WW moved there too.
doRndV03UsingHashSet (moveOrder, allPosSet) =
  -- create new Set from old Set
  (rotMoveOrder moveOrder
  -- [✔] - cycle over all elfPos
  , HashSet.foldr ( \elfPos newSet ->
                  if (canMoveUsingHashSet elfPos allMoves allPosSet) 
                     && (not (isDestDupeHashSet elfPos moveOrder allPosSet))
                    then -- [✔] - store new elfPos
                        HashSet.insert (getNewElfPosHashSet elfPos moveOrder allPosSet) newSet
                    else -- [✔] - canMove == False OR Destination is duplicated
                        -- [✔] - store (old) elfPos
                        HashSet.insert elfPos newSet
            ) (HashSet.empty :: HashSet.HashSet ElfPos) allPosSet )

-- it IS possible that an Elf is *allowed* to move, but cannot!
getNewElfPos ePos moveOrder allPosSet = head $ (<> [ePos]) $ onlyValidMoves ePos moveOrder allPosSet
onlyValidMoves ePos' moveOrder allPosSet
  = foldr (\move acc ->
                        if (canMoveUsingSet ePos' (legalMoves move) allPosSet)
                          then ((moveToTile move) ePos') : acc
                          else acc
          ) [] moveOrder

-- it IS possible that an Elf is *allowed* to move, but cannot!
getNewElfPosHashSet ePos moveOrder allPosSet = head $ (<> [ePos]) $ onlyValidMovesHashSet ePos moveOrder allPosSet
onlyValidMovesHashSet ePos' moveOrder allPosSet
  = foldr (\move acc ->
                        if (canMoveUsingHashSet ePos' (legalMoves move) allPosSet)
                          then ((moveToTile move) ePos') : acc
                          else acc
          ) [] moveOrder

-- [✔] - get NN, SS, EE, and WW and check if (a) and elf is standing there and (b) can move
twoAway ePos allPosSet =
  filter (\ePos' -> (S.member ePos' allPosSet) && (canMoveUsingSet ePos' allMoves allPosSet))
  $ map (flip moveToTile ePos) [NN, SS, EE, WW]

-- [✔] - get NN, SS, EE, and WW and check if (a) and elf is standing there and (b) can move
twoAwayHashSet ePos allPosSet =
  filter (\ePos' -> (HashSet.member ePos' allPosSet) && (canMoveUsingHashSet ePos' allMoves allPosSet))
  $ map (flip moveToTile ePos) [NN, SS, EE, WW]

-- [✔] - check if NN, SS, EE, or WW move to the same tile as the current elfPos (here: ePos)
isDestDupe ePos moveOrder allPosSet
  = (getNewElfPos ePos moveOrder allPosSet)
    `elem` (map (\ePos' -> getNewElfPos ePos' moveOrder allPosSet) $ twoAway ePos allPosSet)

-- [✔] - check if NN, SS, EE, or WW move to the same tile as the current elfPos (here: ePos)
isDestDupeHashSet ePos moveOrder allPosSet
  = (getNewElfPosHashSet ePos moveOrder allPosSet)
    `elem` (map (\ePos' -> getNewElfPosHashSet ePos' moveOrder allPosSet) $ twoAwayHashSet ePos allPosSet)

-- *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** *** 

doTenRoundsPartA01 strFromFile =
  snd $ foldr (\cnt acc -> doRndV01UsingMap acc) (initialMoveOrder, allElfPos) [1..10]
  -- snd $ foldr (\cnt acc -> trace ("calling doRndV01UsingMap with acc = " ++ show acc) $ doRndV01UsingMap acc) (initialMoveOrder, allElfPos) [1..10]
    where
      allElfPos = elfPosFromFile strFromFile

doTenRoundsPartA02 strFromFile =
  S.toList $ snd $ foldr (\cnt acc -> doRndV02UsingSet acc) (initialMoveOrder, allElfPos) [1..10]
  -- S.toList $ snd $ foldr (\cnt acc -> trace ("calling doRndV02UsingSet with acc = " ++ show acc) $ doRndV02UsingSet acc) (initialMoveOrder, allElfPos) [1..10]
    where
      allElfPos = elfPosFromFileToSet strFromFile

doTenRoundsPartA03 strFromFile =
  HashSet.toList $ snd $ foldr (\cnt acc -> doRndV03UsingHashSet acc) (initialMoveOrder, allElfPos) [1..10]
    where
      allElfPos = elfPosFromFileToHashSet strFromFile

doRoundsPartB moveOrder allPos =
  let prevPos = [] :: [ElfPos]
  in
    foldr (\cnt (rnd, !acc, prevPos') ->
              let !res  = doRndV01UsingMap acc
                  !prev = snd acc
              in
                if ( (rnd == 3) )
                  then (rnd, acc, prevPos')
                  else (cnt, res, prev)
          )
          (0,(moveOrder, allPos), prevPos)
          [1..1000]

doRndsUntilDoneB01 strFromFile = go 0 (initialMoveOrder, allElfPos) []
  where
    go rnd res@(mo, ap) prevPos =
      if rnd == 5 || (ap == prevPos)
        then rnd -- (rnd, res)
        else go (rnd+1) (doRndV01UsingMap res) ap
    allElfPos = elfPosFromFile strFromFile

doRndsUntilDoneB02 strFromFile = go 0 (initialMoveOrder, allElfPos) S.empty
  where
    go rnd res@(mo, ap) prevPos =
      if rnd == 2000 || (ap == prevPos)
        then rnd -- (rnd, res)
        else go (rnd+1) (doRndV02UsingSet res) ap
    allElfPos = elfPosFromFileToSet strFromFile

dispTimings start end = do
  putStrLn $ "Start = " <> show start <> " end = " <> show end <> " Time = " <> show (end-start)
  putStrLn $ "\t ... or " <> show ( fromIntegral (end-start)  /10^9 ) <> " ms"

partA' :: String -> String -> Bool -> (String -> [ElfPos]) -> IO ()
partA' strFromFile verTag showTimings tenRndsFunc = do
  start <- getCPUTime
  let 
      coords = tenRndsFunc strFromFile
      (xs, ys) = unzip coords
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys
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

-- partB :: String -> String -> Bool -> (String -> () -> Int) -> IO ()
partB strFromFile verTag showTimings doRndsFunc = do
  start <- getCPUTime
  -- let numRounds = pB2 initialMoveOrder allPos doRndsFunc
  let numRounds = doRndsFunc strFromFile
  putStrLn $ "Part B, fast enough ???"
  putStrLn $ "Rounds until no movement = " <> show numRounds
  end <- getCPUTime

  if showTimings then dispTimings start end
                 else putStr ""
  putStrLn "------------------------"


main = do
  -- fileStr <- readFile "input-23-test.txt"
  fileStr <- readFile "input-23.txt"

  -- partA f "version 1:" True doTenRoundsPartA01

  -- partA f "version 2:" True doTenRoundsPartA02

  -- partA f "version 3:" True doTenRoundsPartA03

  partA fileStr "version 2 using Set:" True doTenRoundsPartA
        S.toList S.fromList S.foldr S.insert S.empty S.member

  partA fileStr "version 3 using HashSet:" True doTenRoundsPartA
        HashSet.toList HashSet.fromList HashSet.foldr HashSet.insert HashSet.empty HashSet.member

  -- partB f "version 1:" True doRndsUntilDoneB01

  -- partB f "version 2:" True doRndsUntilDoneB02
