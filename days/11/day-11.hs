{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Map as M

{-
    Day 11

      Part A - Monkey Business
        . focus on the two most active monkeys if you want any hope
          of getting your stuff back. Count the total number of times
          each monkey inspects items over 20 rounds

        . what is the result of multiplying the number of items inspected
          by the two most active monkeys?
-}

data Monkey = Monkey {
    mID :: Int
  , items :: [Int]
  , op :: Int -> Int
  , ifT :: Int
  , ifF :: Int
}

showMonkey :: Monkey -> String
showMonkey m = show $ mID m

monkeyInLine :: String -> Bool
monkeyInLine line = "Monkey" `elem` words line

-- happyPathParser :: String -> ???
happyPathParser str = 
  let lns = lines str
      blankMnky = Monkey (-1) [] (+0) (-1) (-1)
  in
    foldl (\mnky ln -> if
              | (monkeyInLine ln) -> blankMnky { mID = 42 }
              | otherwise -> undefined ) blankMnky lns

main :: IO ()
main = do
  f <- readFile "input-11-test.txt"
  -- f <- readFile "input-11.txt"
  
  putStrLn $ replicate 42 '-'

  let parsedInput = happyPathParser f

  putStrLn $ unlines $ take 3 parsedInput