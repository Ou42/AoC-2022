module Main where

import Data.List (transpose)
import qualified Data.Map as Map
import Data.Map (Map(..))

{-
    Day 08

      Part A
        . given a 2D grid of tree heights
              . find the number of trees visible from the outside looking in
              . A tree is visible if all of the other trees between it and
                  an edge of the grid are shorter than it.

      Part B
        . find the tree with the greatest "visibility"
        . where "visibility" is the product of each visible distance before
          being blocked by a tree of equal or greater height in each of the
          4 directions (NSEW/up,down,right,left)

      input-08-test.txt
        30373
        25512
        65332
        33549
        35390
-}

-- Part A

chkRowOfTreesR2L :: String -> [Bool]
chkRowOfTreesR2L row =
    {-
       - build a List of T/F based on whether the Tree is smaller
         than the up to then max height tree
       - start comparison against `firstTestTree` then update testTree
         to be the max of those 2.
       - FIRST & LAST rows should be ALL True
          - they will become all True due to the transposing but is there
            a more efficient way? Taking out the head, yes, but `last`?
    -}
  let firstTestTree = '.' -- Tree "shorter" than '0'
  in
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

areVisiblePartA :: [String] -> Int
areVisiblePartA trees =
  length
  $ filter (== True)
  $ concat
  $ chkAllTrees trees

myShow :: [[Bool]] -> String
myShow visTrees = unlines $ map (map (\a -> if a then 'T' else 'f')) visTrees

-- Part B

{-
    . take a 2D grid of trees ...

      . assign to Left 2 Right ( or West 2 East )
      . take the transpose & assign to Down 2 Up ( South 2 North )

      . moving L 2 R, calc & store the L & R Visibilty Lists & dists
      . calc & store the North & South Visibility Lists & distances

      . where, Linear Visibility is:
            Number of trees until same height or edge

      . calc Total Visibility for current tree
      . if (>) prev max, replace max

      . where, Total Visibility is:
            Product of all 4 Linear Visibility values

      . return max Total Visibility

      NOTE: trees on the outside edge have at least 1 Distance of ZERO
            and would therefore have a Visibility of ZERO. Ignore them!
-}

type Dist = ([Char],Int)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil f trees = go trees
  where
        go [] = []
        go (t:ts) = if f t then [t]
                           else t : go ts

type Row = Int
type Col = Int
type EastWest = Map (Row, Col) (String, String)
type NorthSouth = Map (Row, Col) (String, String)
type TwoDist = Map (Row, Col) Int

-- m - Map.empty :: EastWest

prodOf2Dist :: (String, String) -> Int
prodOf2Dist (l, r) = length l * length r

-- visL2R :: String -> Int
-- visL2R f rowNum treeRow = zip (f zip (repeat rowNum) [0..]) $ go [] treeRow
visL2R f rowNum treeRow = zip (f zip (repeat rowNum) [0..]) $ map prodOf2Dist $ go [] treeRow
  -- let emptyDist = ([],0)
  -- in
      -- foldr (\tree (prevStuff:_) -> ???) (emptyDist,emptyDist) treeRow
  -- less efficient first. Get it done!
  where
        go :: String -> String -> [(String, String)]
        go accu []     = []
        go accu (t:ts) = ((takeUntil (>= t) accu), (takeUntil (>= t) ts)) : go (t:accu) ts

-- genAllVisL2R :: [String] -> EastWest
genAllVisL2R :: [String] -> TwoDist
genAllVisL2R trees = go 0 (Map.empty) trees
  where 
        go _ allVisL2R [] = allVisL2R
        go row allVisL2R (tRow:tRs) = go (row+1) (Map.union allVisL2R $ Map.fromList $ visL2R id row tRow) tRs

partB trees = maximum . snd . unzip $ Map.toList $ go 0 (allVisL2R) treesNS
  where
        allVisL2R = genAllVisL2R trees
        treesNS   = transpose trees

        go _ allVis [] = allVis
        go row allVis (tRow:tRs) = go (row+1) (Map.unionWith (*) allVis $ Map.fromList $ visL2R flip row tRow) tRs


coordL2R f rowNum treeRow = zip (f zip (repeat rowNum) [0..]) treeRow

genAllL2R :: [String] -> Map (Row, Col) Char
genAllL2R trees = go 0 (Map.empty) trees
  where 
        go _ allL2R [] = allL2R
        go row allL2R (tRow:tRs) = go (row+1) (Map.union allL2R $ Map.fromList $ coordL2R id row tRow) tRs

{-
    *Main> f <- readFile "input-08-test.txt" 
    *Main> ts = lines f
    *Main> ts
    ["30373","25512","65332","33549","35390"]
    *Main> tts
    ["32633","05535","35353","71349","32290"]
    *Main> coordL2R id 0 (head ts)
    [((0,0),'3'),((0,1),'0'),((0,2),'3'),((0,3),'7'),((0,4),'3')]
    *Main> coordL2R flip 0 (head tts)
    [((0,0),'3'),((1,0),'2'),((2,0),'6'),((3,0),'3'),((4,0),'3')]
    *Main> :t id
    id :: a -> a
    *Main> :t flip
    flip :: (a -> b -> c) -> b -> a -> c
    *Main> :t id zip
    id zip :: [a] -> [b] -> [(a, b)]
    *Main> :t flip zip
    flip zip :: [b] -> [a] -> [(a, b)]


    *Main> putStrLn $ unlines ts
    30373
    25512
    65332
    33549
    35390

    ** NOTE: the following are now `prodOf2Dist` values! **

    *Main> take 5 $ Map.toList all
    [((0,0),0),((0,1),1),((0,2),2),((0,3),3),((0,4),0)]
    *Main> take 5 $ drop 5 $ Map.toList all
    [((1,0),0),((1,1),1),((1,2),2),((1,3),1),((1,4),0)]
    *Main> take 5 $ drop 10 $ Map.toList all
    [((2,0),0),((2,1),3),((2,2),1),((2,3),1),((2,4),0)]
    *Main> take 5 $ drop 15 $ Map.toList all
    [((3,0),0),((3,1),1),((3,2),4),((3,3),1),((3,4),0)]
    *Main> take 5 $ drop 20 $ Map.toList all
    [((4,0),0),((4,1),2),((4,2),1),((4,3),3),((4,4),0)]

-}

-- ** NOT sure if flip-ing the zip is correct. Need Row, Col to match up for L2R and NS **

main :: IO ()
main = do
  -- f <- readFile "input-08.txt"
  f <- readFile "input-08-test.txt"

  let trees = lines f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ take 5 $ trees
  putStrLn $ replicate 42 '-'

  putStrLn $ trees !! 0

  putStrLn "Although `zip (0:b) b` is cool, I don't think it works for the whole row"
  putStrLn "Maybe if it short-circuits, but not `map` over the entire row"

  putStrLn $ replicate 42 '-'

  putStrLn "Idea is to L -> R on each row creating a 2D grid of T/F"
  putStrLn "then feed in the reverse (R -> L), and the 2 transposed directions"
  putStrLn "then take the 4 2D (T/F) and OR them together, then sum the T's"
  
  putStrLn $ replicate 42 '-'

  putStrLn $ myShow $ chkAllTrees trees

  putStrLn $ replicate 42 '-'
  
  putStrLn $ "Answer for Part A ( for 'test' should be 21 ) == " ++ (show $ areVisiblePartA trees)
  