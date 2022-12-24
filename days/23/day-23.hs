module Day23 where
import qualified Data.Set as S
import qualified Data.Map as M

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

allMoves :: [Moves]
allMoves = [N, S, W, E, NW, NE, SW, SE]

initialMoveOrder :: [Moves]
initialMoveOrder = [N, S, W, E]

rotMoveOrder :: [Moves] -> [Moves]
rotMoveOrder (h:t) = t <> [h]

canMove elfPos moves allPos =
  any (==True)
  $ map ( (flip elem allPos)
        . (flip moveFuncs elfPos)
        ) moves

-- create a Map to check if 2+ elves landed on same tile
-- k v == (new pos) [(old pos)]
doRound moveOrder allPos = 
  let mapPos :: M.Map ElfPos [ElfPos]
      mapPos = M.empty
  in
    map ( id
        ) allPos


main = do
  f <- readFile "input-23-test.txt"

  putStrLn "-- raw elf position data:"
  putStrLn f

  let allElfPos = elfPosFromFile f
  putStrLn "-- Parse of elf position data:"
  putStrLn $ show allElfPos
