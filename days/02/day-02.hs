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