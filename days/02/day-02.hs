{-
    https://adventofcode.com/2022/day/2

       
    (A) RPS Tournament
        2 columns of letters given:
        . col 1: [A,B,C] == Rock, Paper, Sissors
        . col 2: [X,Y,Z] == Rock, Paper, Sissors
        
        . [X,Y,Z] == [1,2,3]

        . [lose, draw, win] == [0,3,6]

        . score each round: [X,Y,Z] + [lose, draw, win]
        . return total of all the rounds 

-}

getRPS :: String -> [(Char, Char)]
getRPS f =
  map (\r -> (toXYZ $ head r, r !! 2)) $ lines f
  where
    toXYZ 'A' = 'X'
    toXYZ 'B' = 'Y'
    toXYZ 'C' = 'Z'
    
day02PartA :: [(Char, Char)] -> String
day02PartA rs =
  let scores = map (\(op, me) -> valXYZ me + score op me) rs
  in "Day 02 - Part A: Total Score = " <> show (sum scores)
  where
    valXYZ 'X' = 1
    valXYZ 'Y' = 2
    valXYZ 'Z' = 3
    score 'X' 'Y' = 6
    score 'X' 'Z' = 0
    score 'Y' 'X' = 0
    score 'Y' 'Z' = 6
    score 'Z' 'X' = 6
    score 'Z' 'Y' = 0
    score _ _ = 3

main = do
  f <- readFile "input-02.txt"
  putStrLn $ day02PartA $ getRPS f
