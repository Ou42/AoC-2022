module Main where

{-
    Day 10

      Part A
        . Given a list of ASM-like cmds determine the value of a register X
          at certain cycles...

        . "Start by figuring out the signal being sent by the CPU. The CPU has
           a single register, X, which starts with the value 1. It supports only
           two instructions:

             - addx V takes two cycles to complete. After two cycles, the X register
               is increased by the value V. (V can be negative.)
             - noop takes one cycle to complete. It has no other effect."

        . "Consider the signal strength (the cycle number multiplied by the value of
           the X register) during the 20th cycle and every 40 cycles after that (that
           is, during the 20th, 60th, 100th, 140th, 180th, and 220th cycles)."

        . keyword: ***during*** ( not after )

        . I suspect an "off by one" error INCOMING!

        . As a first (quick?!) iteration on the problem, I will model
          the commands as a `Maybe Int`:
            . noop     -> Nothing
            . addx Num -> Just Num
-}

{-
    Using day-10-test.hs

    The interesting signal strengths can be determined as follows:

    - During the  20th cycle, register X has the value 21, so the signal strength is  20 * 21 =  420.
        (The 20th cycle occurs in the middle of the second addx -1, so the value of register X is the
         starting value, 1, plus all of the other addx values up to that point:
         1 + 15 - 11 + 6 - 3 + 5 - 1 - 8 + 13 + 4 = 21.)
    - During the  60th cycle, register X has the value 19, so the signal strength is  60 * 19 = 1140.
    - During the 100th cycle, register X has the value 18, so the signal strength is 100 * 18 = 1800.
    - During the 140th cycle, register X has the value 21, so the signal strength is 140 * 21 = 2940.
    - During the 180th cycle, register X has the value 16, so the signal strength is 180 * 16 = 2880.
    - During the 220th cycle, register X has the value 18, so the signal strength is 220 * 18 = 3960.

    The sum of these signal strengths is 13140.
-}

parseInput :: String -> [Maybe Int]
parseInput fileInput =
  let lns = lines fileInput
      parseLine :: String -> Maybe Int
      parseLine str =
        case str of
          "noop" -> Nothing
          -- _      -> Just (read $ snd $ break (==' ') str :: Int)
          -- _      -> Just (read $ dropWhile (not . (== ' ')) str :: Int)
          _      -> Just (read $ dropWhile (/= ' ') str :: Int)
 
  in
     map parseLine lns

-- Part A
  -- tbd

main :: IO ()
main = do
  f <- readFile "input-10-test.txt"

  let cmds = parseInput f

  putStrLn $ replicate 42 '-'
  putStrLn $ unlines $ map show $ take 15 cmds
  -- putStrLn $ replicate 42 '-'
