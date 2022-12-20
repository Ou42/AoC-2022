import Data.List (sortBy)
import Data.List.Split (splitOn)

{-
    https://adventofcode.com/2022/day/1

    Translation:

        Given a list of grouped numbers...

        (A) find the group w/ the greatest sum

        (B) find the top 3 groups, return their sum
-}

getCalsPerElf :: String -> [[Int]]
getCalsPerElf f =
  let calPerElf = splitOn "\n\n" f
      calPerLn  = map lines calPerElf
  in  (map (map read) calPerLn) :: [[Int]]

day01PartA :: [[Int]] -> String
day01PartA css =
  -- (A) find the group w/ the greatest sum
  let calSums = map sum css
  in "Day 01 - Part A: The max total calories = " <> show (maximum calSums)

day01PartB :: [[Int]] -> String
day01PartB css =
  -- (B) find the top 3 groups, return their sum
  let calSums = map sum css
      top3 = sum $ take 3 $ sortBy (flip compare) calSums
  in "Day 01 - Part B: Sum of top 3 calories = " <> show top3

main = do
  f <- readFile "input-01.txt"
  let css = getCalsPerElf f

  putStrLn $ day01PartA css

  putStrLn $ day01PartB css
