module Day09B where

{-
    Part A

        Alg for (moving) H & T

        Function moveHeadTail' (H) a Direction & Distance

        moveHeadTail :: Knot -> Knot -> Dir -> (Knot, Knot)
        moveHeadTail h t dir =
            -- Remember Head position.
            let oldH = h
                newH = moveHead h dir
            in
                If (adjacent newH t) then (newH, t)
                                     else (newH, oldH)




-}