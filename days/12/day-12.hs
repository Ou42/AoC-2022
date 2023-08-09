{-# LANGUAGE StrictData #-}

module Main where

{-
    --- Day 12: Hill Climbing Algorithm --- Part A

    You try contacting the Elves using your handheld device, but the
    river you're following must be too low to get a decent signal.

    You ask the device for a heightmap of the surrounding area (your
    puzzle input). The heightmap shows the local area from above broken
    into a grid; the elevation of each square of the grid is given by a
    single lowercase letter, where a is the lowest elevation, b is the
    next-lowest, and so on up to the highest elevation, z.

    Also included on the heightmap are marks for your current position (S)
    and the location that should get the best signal (E). Your current
    position (S) has elevation a, and the location that should get the best
    signal (E) has elevation z.

    You'd like to reach E, but to save energy, you should do it in as few
    steps as possible. During each step, you can move exactly one square
    up, down, left, or right. To avoid needing to get out your climbing
    gear, the elevation of the destination square can be at most one higher
    than the elevation of your current square; that is, if your current
    elevation is m, you could step to elevation n, but not to elevation o.
    (This also means that the elevation of the destination square can be
    much lower than the elevation of your current square.)

    For example:

    Sabqponm
    abcryxxl
    accszExk
    acctuvwj
    abdefghi

    Here, you start in the top-left corner; your goal is near the middle.
    You could start by moving down or right, but eventually you'll need
    to head toward the e at the bottom. From there, you can spiral around
    to the goal:

    v..v<<<<
    >v.vv<<^
    .>vv>E^^
    ..v>>>^^
    ..>>>>>^

    In the above diagram, the symbols indicate whether the path exits each
    square moving up (^), down (v), left (<), or right (>). The location
    that should get the best signal is still E, and . marks unvisited squares.

    This path reaches the goal in 31 steps, the fewest possible.

    What is the fewest steps required to move from your current position to
    the location that should get the best signal?
-}

import Control.Monad ((>=>))
import Data.Map (Map)
import qualified Data.Map as M 
import Data.Maybe ( fromJust, fromMaybe ) 
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Console.Terminfo (Point(row))

type Array2D = Vector (Vector Char)
type Visited = Map (Int, Int) Bool

createArray2D :: Int -> Int -> Array2D
createArray2D n m = V.replicate n (V.replicate m '.')


array2DfromString :: String -> Array2D
array2DfromString str =
  let rows = lines str
      vecRows = map V.fromList rows
  in
      V.fromList vecRows


getElevRow :: Int -> Array2D -> Maybe (Vector Char)
-- getElevRow row arr2d = arr2d V.!? row
-- pointfree ver via Bing
getElevRow row = (V.!? row)


getElevation :: Int -> Int -> Array2D -> Maybe Char
-- >=> (aka "fish") is the Kleisli composition operator,
--     which is used to compose two functions that return monadic values
-- (!?) :: Vector a -> Int -> Maybe a
--     O(1) Safe indexing.
getElevation row col = (V.!? row) >=> (V.!? col)


hasAvailableNextStep :: Int -> Int -> Array2D -> Bool
-- doesn't take into consideration 'S' and 'E'
-- ... will need to convert them for currElevation & possibleNextSteps
hasAvailableNextStep row col arr2D = 
  let currElevation = fromJust $ getElevation row col arr2D
      possibleNextSteps = [ getElevation (row+1) col     arr2D 
                          , getElevation (row-1) col     arr2D 
                          , getElevation row     (col+1) arr2D 
                          , getElevation row     (col-1) arr2D 
                          ]
  in
      -- possibleNextSteps
      any ((<=succ currElevation) . fromMaybe 'z') possibleNextSteps


deadEnds :: Array2D -> Vector (Vector Bool)
-- doesn't take into consideration 'S' and 'E' (see above)
deadEnds arr2D = V.imap (\row a -> V.imap (\col b -> hasAvailableNextStep row col arr2D) a) arr2D
-- deadEnds = V.imap (\row a -> row)

-- findInArray :: Char -> Array2D -> (Int, Int)
findInArray chr arr2D = 
  let row = fromJust $ V.findIndex (V.any (==chr)) arr2D
      rowVec = fromJust $ arr2D V.!? row
      col = fromJust $ V.elemIndex chr rowVec
  in
      (row, col)

-- findStart :: undefined
findStart = findInArray 'S'

-- findEnd :: undefined
findEnd = findInArray 'E'

main :: IO ()
main = do
  fileInput <- readFile "input-12-test.txt"
  -- fileInput <- readFile "input-12.txt"
  let elevations = array2DfromString fileInput
      start = findStart elevations
      end   = findEnd   elevations

  putStrLn $ replicate 42 '-'

  putStrLn fileInput

  putStrLn $ replicate 42 '-'

--   print $ createArray2D 5 5
  putStrLn $ "Start = " ++ show start
  putStrLn $ "End   = " ++ show end
