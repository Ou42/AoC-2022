{-# LANGUAGE StrictData #-}

module Main where

{-
    Day 12

      Part A - 

-}


main :: IO ()
main = do
  fileInput <- readFile "input-12-test.txt"
  -- fileInput <- readFile "input-12.txt"

  putStrLn $ replicate 42 '-'

  putStrLn fileInput
