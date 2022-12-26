{-# LANGUAGE BangPatterns #-}

module Day23 where
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

-- create a Map to check if 2+ elves landed on same tile
-- k v == (new pos) [(old pos)]
doRound (moveOrder, allPos) = 
  -- map creates a List of Maps.
  -- fold creates ONE Map:
  let mapPossPos = foldr ( \ePos eMap -> M.insertWith (<>) (getNewElfPos ePos) [ePos] eMap ) M.empty allPos
                      where
                        getNewElfPos elfPos = (moveOrNot elfPos) $ (canMove elfPos allMoves allPos)
                        moveOrNot elfPos False = elfPos
                        moveOrNot elfPos True  = newElfPos'
                          where
                            -- it IS possible that an Elf is *allowed* to move, but cannot!
                            newElfPos' = head $ (<> [elfPos]) $ snd $ unzip $ filter ((==True) . fst) $ zip validMoves possMoves
                            validMoves = map (\move -> (canMove elfPos (legalMoves move) allPos)) moveOrder
                            possMoves  = map (\move -> (moveFuncs move) elfPos) moveOrder
      possPos = M.keys mapPossPos

  in
    (rotMoveOrder moveOrder, concatMap (\(possPos', oldPos) -> if (length oldPos) == 1
                                                                 then [possPos']
                                                                 else oldPos)
                             $ M.toList mapPossPos)

doRoundsPartA moveOrder allPos =
  foldr (\cnt acc -> doRound acc) (moveOrder, allPos) [1..10]

doRoundsPartB moveOrder allPos =
  -- the following didn't work and returned (1, ...)
  --    probably due to lazy evaluation?! It just read cnt and exited ???
  -- foldr (\cnt res@(_,acc) -> if cnt == 4 then res else (cnt,(doRound acc))) (0,(moveOrder, allPos)) [1..10]

  -- this *did* do the calculations and short-circuited
  -- foldr (\cnt (rnd,acc) -> if rnd == 3 then (rnd, acc) else (cnt,(doRound acc))) (0,(moveOrder, allPos)) [1..10]

  -- check to see if no movement: slow method(?) compare 2 rounds
{-
  -- foldr (\cnt (rnd,acc@(_,allPos')) -> if ((allPos' == (snd (doRound acc))) || (rnd == 777))
  foldr (\cnt (rnd,acc@(_,allPos')) -> if ( (rnd == 777) || (allPos' == (snd (doRound acc))) )
                             then (rnd, acc)
                             else (cnt,(doRound acc)))
        (0,(moveOrder, allPos))
        [1..1000]
-- why does it go to "982" if I'm trying to short-circuit at 777 ?!        
-- I don't know nuthin'! ... YET!
-- ... but I'm getting closer ... (982,([E,N,S,W],[(0,5),(2,2),(2,7),(3,4),(4,1),(4,6),(4,8),(4,10),(6,3),(6,8),(7,0),(7,11),(8,2),(8,4),(8,7),(9,9),(10,4),(10,6),(11,1),(11,8),(11,10),(13,4)]))
-}
  let prevPos = [] :: [ElfPos]
  in
    foldr (\cnt (rnd, !acc, prevPos') ->
              let !res  = doRound acc
                  !prev = snd acc
              in
                if ( (rnd == 3) )
                  then (rnd, acc, prevPos')
                  else (cnt, res, prev)
          )
          (0,(moveOrder, allPos), prevPos)
          [1..1000]

partA :: [ElfPos] -> IO ()
partA allPos = do
  let tenRounds = doRoundsPartA initialMoveOrder allPos

      coords = snd tenRounds
      (xs, ys) = unzip coords
      minX = minimum xs
      maxX = maximum xs
      minY = minimum ys
      maxY = maximum ys

  putStrLn $ "minmax X = " <> show minX <> " " <> show maxX
  putStrLn $ "minmax Y = " <> show minY <> " " <> show maxY

  let area = ((maxX - minX + 1) * (maxY - minY + 1))
  putStr $ show area
  putStrLn " (The minimum orthogonal rectangular area)"

  let numElves = (length coords)
  putStrLn $ "minus " <> show numElves <> " (the # of elves)"
  putStrLn "======"
  putStrLn $ show (area - numElves) <> " (The answer for Part A)"

partB :: [ElfPos] -> IO ()
partB allPos = do
  let getRounds = doRoundsPartB initialMoveOrder allPos
  putStrLn "I don't know nuthin'! ... YET!"
  putStrLn $ "... but I'm getting closer ... "
  let (a,(b,c),d) = getRounds
  print a
  print b
  print c
  print d
{-
1
[N,S,W,E]
[(0,5),(2,2),(2,7),(3,4),(4,1),(4,6),(4,8),(4,10),(6,3),(6,8),(7,0),(7,11),(8,2),(8,4),(8,7),(9,9),(10,4),(10,6),(11,1),(11,8),(11,10),(13,4)]
[(0,5),(2,2),(2,7),(3,4),(4,1),(4,6),(4,8),(4,10),(6,3),(6,8),(7,0),(7,11),(8,2),(8,4),(8,7),(9,9),(10,4),(10,6),(11,1),(11,8),(11,10),(13,4)]

2
[E,N,S,W]
[(0,5),(2,2),(2,7),(3,4),(4,1),(4,6),(4,8),(4,10),(6,3),(6,8),(7,0),(7,11),(8,2),(8,4),(8,7),(9,9),(10,4),(10,6),(11,1),(11,8),(11,10),(13,4)]
[(0,5),(2,2),(2,7),(3,4),(4,1),(4,6),(4,8),(4,10),(6,3),(6,8),(7,0),(7,11),(8,2),(8,4),(8,7),(9,9),(10,4),(10,6),(11,1),(11,8),(11,10),(13,4)]
-}


main = do
  f <- readFile "input-23-test.txt"
  -- f <- readFile "input-23.txt"

  -- putStrLn "-- raw elf position data:"
  -- putStrLn f

  let allElfPos = elfPosFromFile f
  -- putStrLn "-- Parse of elf position data:"
  -- putStrLn $ show allElfPos

  -- putStrLn ""
  -- partA allElfPos

  -- putStrLn ""
  partB allElfPos
