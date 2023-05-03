module Day09B where

import Prelude hiding (Left, Right) 

{-
    Part A

        Alg for (moving) H & T
-}

data Dir = Left | Right | Up | Down

data Knot = Knot { row :: Int, col :: Int }

-- Function moveHeadTail' (H) a Direction & Distance

moveHeadTail :: Knot -> Knot -> Dir -> (Knot, Knot)
moveHeadTail h t dir =
    let oldH = h
        newH = moveHead h dir
    in
        if adjacent newH t then (newH, t)
                           else (newH, oldH)

moveHead :: Knot -> Dir -> Knot
moveHead h Left  = h { row = row h- 1 }
moveHead h Right = h { row = row h + 1 }
moveHead h Up    = h { col = col h + 1 }
moveHead h Down  = h { col = col h - 1 }

adjacent :: Knot -> Knot -> Bool
adjacent _ _ = True




