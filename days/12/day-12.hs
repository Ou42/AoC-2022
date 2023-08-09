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
import Data.Maybe ( fromJust, fromMaybe )
import Data.Set (Set)
import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import System.Console.Terminfo (Point(row))
import qualified Control.Applicative as Set

type Array2D = Vector (Vector Char)
type Visited = Set (Int, Int)

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


getElevation :: (Int, Int) -> Array2D -> Maybe Char
-- converts 'S' to 'a' & 'E' to 'z'
-- >=> (aka "fish") is the Kleisli composition operator,
--     which is used to compose two functions that return monadic values
-- (!?) :: Vector a -> Int -> Maybe a
--     O(1) Safe indexing.
getElevation (row, col) arr2D = go $ ((V.!? row) >=> (V.!? col)) arr2D
  where
    go Nothing = Nothing
    go (Just c) = case c of
      'S' -> Just 'a'
      'E' -> Just 'z'
      _   -> Just c


hasAvailableNextStep :: (Int, Int) -> Array2D -> Bool
-- getElevation converts 'S' to 'a' & 'E' to 'z'
hasAvailableNextStep (row, col) arr2D = 
  let currElevation = fromJust $ getElevation (row, col) arr2D
      possibleNextSteps = [ getElevation (row+1, col) arr2D 
                          , getElevation (row-1, col) arr2D 
                          , getElevation (row, col+1) arr2D 
                          , getElevation (row, col-1) arr2D 
                          ]
  in
      -- possibleNextSteps
      any ((<=succ currElevation) . fromMaybe 'z') possibleNextSteps


possNextSteps :: (Int, Int) -> [(Int, Int)]
possNextSteps (row, col) = [(row+1, col), (row-1, col), (row, col+1), (row, col-1)]


isValidElevation :: (Int, Int) -> (Int, Int) -> Array2D -> Bool
isValidElevation currPos nextPos arr2D =
  let currElevation = fromJust $ getElevation currPos arr2D
      nextElevation = fromMaybe 'z' $ getElevation nextPos arr2D
  in
      nextElevation <= succ currElevation


validMoves :: (Int, Int) -> Array2D -> Set (Int, Int) -> [(Int, Int)]
validMoves currPos arr2D visited = filter (\possPos -> S.notMember possPos visited
                                                       &&
                                                       isValidElevation currPos possPos arr2D) possPs
  where
    possPs = possNextSteps currPos

deadEnds :: Array2D -> Vector (Vector Bool)
-- getElevation ( called by hasAvailableNextStep ) converts 'S' to 'a' & 'E' to 'z'
deadEnds arr2D = V.imap (\row a -> V.imap (\col b -> hasAvailableNextStep (row, col) arr2D) a) arr2D


findInArray :: Char -> Array2D -> (Int, Int)
findInArray chr arr2D = 
  let row = fromJust $ V.findIndex (V.any (==chr)) arr2D
      rowVec = fromJust $ arr2D V.!? row
      col = fromJust $ V.elemIndex chr rowVec
  in
      (row, col)


findStart :: Vector (Vector Char) -> (Int, Int)
findStart = findInArray 'S'


findEnd :: Vector (Vector Char) -> (Int, Int)
findEnd = findInArray 'E'


findPath :: String -> [(Int, Int)]
findPath fileInput =
  let elevationArr2D = array2DfromString fileInput
      start = findStart elevationArr2D
      end   = findEnd elevationArr2D
      go :: [(Int, Int)] -> [(Int, Int)] -> Visited -> [(Int, Int)]
      go [] solutionPath _ = solutionPath -- this might be where to backtrack?!
      go (currPos:todo) solutionPath visited =
        if currPos == end
          then  end:solutionPath
          else 
                let newSolPath     = currPos : solutionPath
                    newVisited     = S.insert currPos visited
                    validNextSteps = validMoves currPos elevationArr2D newVisited
                in
                    -- want to add the valid next steps to todo
                    -- update solutionPath and visited
                    go (validNextSteps ++ todo) newSolPath newVisited
  in
      go [start] [] S.empty

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
