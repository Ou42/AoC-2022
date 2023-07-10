module Main where

{-
    Day 11

      Part A - Monkey Business
        . focus on the two most active monkeys if you want any hope
          of getting your stuff back. Count the total number of times
          each monkey inspects items over 20 rounds

        . what is the result of multiplying the number of items inspected
          by the two most active monkeys?
-}

main :: IO ()
main = do
  -- f <- readFile "input-11-test.txt"
  f <- readFile "input-11.txt"

  putStrLn $ replicate 42 '-'
