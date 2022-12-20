{-
    https://adventofcode.com/2022/day/2

       
    (A) RPS Tournament
        2 columns of letters given:
        . col 1: [A,B,C] == Rock, Paper, Scissors
        . col 2: [X,Y,Z] == Rock, Paper, Scissors
        
        . [X,Y,Z] == [1,2,3]

        . [lose, draw, win] == [0,3,6]

        . score each round: [X,Y,Z] + [lose, draw, win]
        . return total of all the rounds 

    (B) col 2 has a different meaning ...
        the second column says how the round needs to end:
        . X means you need to lose
        . Y means you need to end the round in a draw
        . Z means you need to win.

        . scoring is still the same, after calculating
          which [Rock, Paper, Scissors] is chosen
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

day02PartB :: [(Char, Char)] -> String
day02PartB rs =
  let scores = map (\(op, col2)
                    -> valXYZ (myRPS op col2)
                    + score col2) rs
  in "Day 02 - Part B: Total Score = " <> show (sum scores)
  where
    valXYZ 'X' = 1
    valXYZ 'Y' = 2
    valXYZ 'Z' = 3
    myRPS op 'Y' = op
    myRPS 'X' 'X' = 'Z'
    myRPS 'Y' 'X' = 'X'
    myRPS 'Z' 'X' = 'Y'
    myRPS 'X' 'Z' = 'Y'
    myRPS 'Y' 'Z' = 'Z'
    myRPS 'Z' 'Z' = 'X'
    
    
    score 'X' = 0
    score 'Y' = 3
    score 'Z' = 6

main = do
  f <- readFile "input-02.txt"
  putStrLn $ day02PartA $ getRPS f
  putStrLn $ day02PartB $ getRPS f
