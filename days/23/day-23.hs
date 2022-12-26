module Day23 where
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

rotMoveOrder :: ([Bool] -> Bool,[Moves]) -> ([Bool] -> Bool,[Moves])
rotMoveOrder (f, (h:t)) = (f, t <> [h])

canMove elfPos (mvChk, moves) allPos =
  mvChk
  $ map ( (flip elem allPos)
        . (flip moveFuncs elfPos)
        ) moves

-- create a Map to check if 2+ elves landed on same tile
-- k v == (new pos) [(old pos)]
doRound moveOrder allPos = 
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
    -- possPos
    -- M.toList mapPossPos
    -- concatMap (mapPossPos ! possPos) possPos
    concatMap (\(possPos', oldPos) -> if (length oldPos) == 1 then [possPos'] else oldPos)
               $ M.toList mapPossPos

doRounds moveOrder allPos =
  doRound moveOrder allPos

main = do
  f <- readFile "input-23-test.txt"

  putStrLn "-- raw elf position data:"
  putStrLn f

  let allElfPos = elfPosFromFile f
  putStrLn "-- Parse of elf position data:"
  putStrLn $ show allElfPos

  putStrLn ""
  putStrLn $ show $ doRounds initialMoveOrder allElfPos
