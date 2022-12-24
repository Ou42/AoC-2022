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
  let -- mapPos :: M.Map ElfPos [ElfPos]
      mapPos = M.empty
    --   moveOrNot :: ElfPos -> Bool -> M.Map ElfPos [ElfPos]
    --   moveOrNot elfPos False = M.insert elfPos [elfPos] mapPos
      moveOrNot elfPos True  =
        -- take the head of the fold of ...
        -- M.insert elfPos [elfPos] mapPos
        M.insertWith (<>) newElfPos [elfPos] mapPos
        where
          validMoves = map (\move -> (canMove elfPos (legalMoves move) allPos)) moveOrder
          possMoves  = map (\move -> (moveFuncs move) elfPos) moveOrder
          newElfPos  = filter ((==True) . fst) $ zip validMoves possMoves

  in
    map ( \ep -> (moveOrNot ep) $ (canMove ep allMoves allPos) ) allPos


main = do
  f <- readFile "input-23-test.txt"

  putStrLn "-- raw elf position data:"
  putStrLn f

  let allElfPos = elfPosFromFile f
  putStrLn "-- Parse of elf position data:"
  putStrLn $ show allElfPos

  putStrLn ""
  putStrLn $ show $ doRound initialMoveOrder allElfPos
