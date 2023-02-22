module Main where

-- import Data.Char (isAlpha)
import qualified Data.Set as Set

{-
    Day 06

      Part A
        . given a String (List of Chars)
        . How many characters need to be processed before the
          first start-of-packet marker is detected?
        . where the 'start-of-packet' marker immediately follows
          the first set of 4 unique, sequential chars.
-}

-- simple: not the most efficient
-- & rather trusting: assumes marker exists!
findUnique4 :: String -> Int
findUnique4 str = go 4 (take 4 str) (drop 4 str)
  where
    go :: Int -> String -> String -> Int
    go idx marker rest =
      let set = Set.fromList marker
          len = length set
      in
        if len == 4
          then idx
          else go (idx + 1) ((drop 1 marker) ++ [head rest]) (tail rest)

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
  putStrLn $ "\t" ++ show (findUnique4 f)
