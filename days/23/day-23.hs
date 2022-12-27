{-# LANGUAGE BangPatterns #-}

module Day23 where
import qualified Data.Set as S
import qualified Data.Map.Strict as M
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
type ElfPosSet = S.Set ElfPos

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

data Moves =   N  | S  | E  | W
             | NW | NE | SW | SE deriving Show

moveFuncs N  = \(x,y) -> (x,y-1)
moveFuncs S  = \(x,y) -> (x,y+1)
moveFuncs E  = \(x,y) -> (x+1,y)
moveFuncs W  = \(x,y) -> (x-1,y)
moveFuncs NE = \(x,y) -> (x+1,y-1)
moveFuncs NW = \(x,y) -> (x-1,y-1)
moveFuncs SE = \(x,y) -> (x+1,y+1)
moveFuncs SW = \(x,y) -> (x-1,y+1)

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

canMove elfPos (mvChk, moves) allPos =
  mvChk
  $ map ( (flip elem allPos)
        . (flip moveFuncs elfPos)
        ) moves

-- per: https://wiki.haskell.org/Foldr_Foldl_Foldl'
-- foldl' f z []     = z
-- foldl' f z (x:xs) = let z' = z `f` x 
--                     in seq z' $ foldl' f z' xs

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
                                                              then ((moveFuncs move) elfPos) : acc
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
  , S.foldr ( \elfPos newSet -> S.insert elfPos newSet
                              --  ************************************
                              --
                              --     - cycle over all elfPos
                              --     - canMove == False
                              --           - store (old) elfPos
                              --           - break/continue looping
                              --     - canMove == True
                              --           - calc new elfPos
                              --           - check NN, SS, EE, or WW
                              --           - if ANY move to new elfPos
                              --                 - store (old) elfPos
                              --           - else
                              --                 - store new elfPos
                              --   
                              --   ************************************
              ) S.empty allPosSet )

doTenRoundsPartA01 strFromFile =
  snd $ foldr (\cnt acc -> doRndV01UsingMap acc) (initialMoveOrder, allElfPos) [1..10]
    where
      allElfPos = elfPosFromFile strFromFile

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

pB2 moveOrder allPos = go 0 (moveOrder, allPos) []
  where
    go rnd res@(mo, ap) prevPos =
      if rnd == 420 || (ap == prevPos)
        then (rnd, res)
        else go (rnd+1) (doRndV01UsingMap res) ap

dispTimings start end = do
  putStrLn $ "Start = " <> show start <> " end = " <> show end <> " Time = " <> show (end-start)
  putStrLn $ "\t ... or " <> show ( fromIntegral (end-start)  /10^9 ) <> " ms"

partA :: String -> String -> Bool -> (String -> [ElfPos]) -> IO ()
partA strFromFile verTag showTimings tenRndsFunc = do
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

partB :: [ElfPos] -> IO ()
partB allPos = do
  -- let getRounds = doRoundsPartB initialMoveOrder allPos
  let getRounds = pB2 initialMoveOrder allPos
  putStrLn $ "Part B, but to slow to find the right answer = ??? = ..."
  print getRounds


main = do
  -- f <- readFile "input-23-test.txt"
  f <- readFile "input-23.txt"

  partA f "version 1:" True doTenRoundsPartA01

  -- partB allElfPos
