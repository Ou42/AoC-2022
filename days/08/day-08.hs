module Main where

import Data.List (transpose)
-- import Debug.Trace (trace)
-- import qualified Data.Map.Lazy as Map
-- import Data.Map.Lazy (Map)
-- import Data.Maybe (fromJust)

{-
    Day 08

      Part A
        . given a 2D grid of tree heights
              . find the number of trees visible from the outside looking in
              . A tree is visible if all of the other trees between it and
                  an edge of the grid are shorter than it.
-}

-- Part A

-- chkRowOfTreesR2L :: String -> [Bool]
chkRowOfTreesR2L row =
  let firstTestTree = '.' -- Tree "shorter" than '0'
  in
    -- [True]
    -- build a List of T/F based on whether the Tree is smaller
    -- than the up to then max height tree
    -- start comparison against `firstTestTree` then update testTree
    -- to be the max of those 2.
    -- FIRST & LAST rows should be ALL True
    -- ... they will become all True due to the tranposing but is
    -- ... there are more efficient way? Taking out the head, yes, but `last`?
    fst $ foldr (\a b -> ((a > (snd b)):(fst b),(max a (snd b)))) ([], firstTestTree) row

zip2DWithOR ts1 ts2 = go [] ts1 ts2
  where
    go accu [] [] = reverse accu
    go accu (t1r:t1rest) (t2r:t2rest) = go ((zipWith (||) t1r t2r):accu) t1rest t2rest

chkAllTrees :: [String] -> [[Bool]]
chkAllTrees trees =
  let 
      treesL2R = map reverse trees
      treesRot = transpose trees
      treesBth = map reverse treesRot

      chkR2L   = map chkRowOfTreesR2L trees
      chkL2R   = map reverse $ map chkRowOfTreesR2L treesL2R
      chkRot   = transpose $ map chkRowOfTreesR2L treesRot
      chkBth   = transpose $ map reverse $ map chkRowOfTreesR2L treesBth

  in
      zip2DWithOR (zip2DWithOR chkR2L chkL2R)
                  (zip2DWithOR chkRot chkBth)

  -- in chkR2L
     -- chkRot chkBth

areVisiblePartA :: [String] -> Int
areVisiblePartA trees =
  length
  $ filter (== True)
  $ concat
  $ chkAllTrees trees

myShow :: [[Bool]] -> [[Char]]
myShow visTrees = map (map (\a -> if a then 'T' else 'f')) visTrees

main :: IO ()
main = do
  f <- readFile "input-08.txt"
  -- f <- readFile "input-08-test.txt"

  let trees = lines f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ take 5 $ trees
  putStrLn $ replicate 42 '-'

  putStrLn $ trees !! 0

  -- putStrLn $ foldr () []
  putStrLn "Although `zip (0:b) b` is cool, I don't think it works for the whole row"
  putStrLn "Maybe if it short-circuits, but not `map` over the entire row"

  putStrLn $ replicate 42 '-'

  putStrLn "Idea is to L -> R on each row creating a 2D grid of T/F"
  putStrLn "then feed in the reverse (R -> L), and the 2 transposed directions"
  putStrLn "then take the 4 2D (T/F) and OR them together, then sum the T's"
  
  putStrLn $ replicate 42 '-'

  putStrLn $ unlines $ myShow $ chkAllTrees trees

  putStrLn $ replicate 42 '-'
  
  putStrLn $ "Answer ( for 'test' should be 21 ) == " ++ (show $ areVisiblePartA trees)
  