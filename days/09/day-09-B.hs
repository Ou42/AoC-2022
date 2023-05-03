module Day09B where

import Prelude hiding (Left, Right) 

-- Part A - Alg for (moving) H & T

data Dir = Left | Right | Up | Down

type Knot = (Int, Int)

moveHeadTailDist :: Knot -> Knot -> Dir -> Int -> (Knot, Knot)
moveHeadTailDist h t dir dist =
    take dist $ iterate $ moveHeadTail h t dir


moveHeadTail :: Knot -> Knot -> Dir -> (Knot, Knot)
moveHeadTail h t dir =
    let oldH = h
        newH = moveHead h dir
    in
        if adjacent newH t then (newH, t)
                           else (newH, oldH)

moveHead :: Knot -> Dir -> Knot
moveHead (r, c) Left  = (r-1, c)
moveHead (r, c) Right = (r+1, c)
moveHead (r, c) Up    = (r, c+1)
moveHead (r, c) Down  = (r, c-1)

adjacent :: Knot -> Knot -> Bool
adjacent (r1, c1) (r2, c2) =
    abs (r1 - r2) <= 2 && abs (c1 - c2) <= 2

-- Part B - Alg for (moving) H & a List of T's

moveKnotList :: [Knot] -> [Knot]
moveKnotList (k0:k1:knots) =
    foldl moveHeadTailPair (k0, k1) knots

moveHeadTailPair :: (Knot, Knot) -> Knot -> (Knot, Knot)
moveHeadTailPair (h, t1) t2 =
    moveHeadTail h t1 