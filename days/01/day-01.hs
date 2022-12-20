import Data.List.Split (splitOn)

getCalsPerElf :: String -> [[Int]]
getCalsPerElf f =
  let calPerElf = splitOn "\n\n" f
      calPerLn  = map lines calPerElf
  in  (map (map read) calPerLn) :: [[Int]]

day01PartA :: [[Int]] -> String
day01PartA css =
  let calSums   = map sum css
  in "Day 01 - Part A: The max total calories = " <> show (maximum calSums)
  
main = do
  f <- readFile "input-01.txt"
  let css = getCalsPerElf f
  putStrLn $ show $ head css
  print $ day01PartA css
