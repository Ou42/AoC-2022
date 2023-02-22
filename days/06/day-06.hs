module Main where

-- import Data.Char (isAlpha)
import qualified Data.Set as Set

{-
    Day 06

      Part A
        . given a String (List of Chars)
        . How many characters need to be processed before the
          first start-of-packet marker is detected?
        . ... where the 'start-of-packet' marker immediately
          follows the first set of 4 unique, sequential chars.

      Part B
        . Same as Part A, but ...
        . How many characters need to be processed before
          the first start-of-message marker is detected?
        . ... where A 'start-of-message' marker is just
          like a start-of-packet marker, except it consists
          of 14 distinct characters rather than 4.
-}

-- simple: not the most efficient
-- & rather trusting: assumes marker exists!
findUniqueLenX :: Int -> String -> Int
findUniqueLenX markerLen str =
  go markerLen (take markerLen str) (drop markerLen str)
  where
    go :: Int -> String -> String -> Int
    go idx marker rest =
      let set = Set.fromList marker
          len = length set
      in
        if len == markerLen
          then idx
          else go (idx + 1) ((drop 1 marker) ++ [head rest]) (tail rest)

solvePartA :: String -> Int
solvePartA str = findUniqueLenX 4 str

solvePartB :: String -> Int
solvePartB str = findUniqueLenX 14 str

main :: IO ()
main = do
  f <- readFile "input-06.txt"

  putStrLn ">> first and last few chars from the input file:"
  -- first 10
  putStrLn $ take 10 f

  -- last 10?! whitespace?!
  putStrLn $ "..." ++ drop (length f - 10) f

  putStrLn "Part A"
  putStrLn $ "\tHow many characters need to be processed"
  putStrLn $ "\tbefore the first start-of-packet marker is detected?"
  putStrLn $ "\t" ++ show (solvePartA f)

  putStrLn "\nPart B"
  putStrLn $ "\tHow many characters need to be processed"
  putStrLn $ "\tbefore the first start-of-message marker is detected?"
  putStrLn $ "\t" ++ show (solvePartB f)
