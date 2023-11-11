module Main where

import Data.Char (isDigit)
import Data.Text (pack, replace, unpack)
import Data.Binary.Get (label)

{-
    Day 13

      Part A
      . Given a list of pairs of packets; pairs are separated by a blank line.
      . Identify how many pairs of packets are in the right order via these rules:

      . Packet data consists of lists and integers. Each list starts with [, ends with ],
        and >= 0 CSV (either integers or other lists).
      . Each packet is always a list and appears on its own line.

      . When comparing 2 values, the 1st value is called "left" and the 2nd value, "right".
      . Then:

        . If both values are integers, the lower integer should come first.
          If the left < right, the inputs are in the right order.
          If the left > right, the inputs are not in the right order.
          Otherwise, the inputs are the same integer;
          continue checking the next part of the input.
        . If both values are lists, compare the first value of each list,
          then the second value, and so on. If the left list runs out of items first,
          the inputs are in the right order. If the right list runs out of items
          first, the inputs are not in the right order. If the lists are the same
          length and no comparison makes a decision about the order, continue checking
          the next part of the input.
        . If exactly one value is an integer, convert the integer to a list which
          contains that integer as its only value, then retry the comparison. For
          example, if comparing [0,0,0] and 2, convert the right value to [2]
          (a list containing 2); the result is then found by instead comparing
          [0,0,0] and [2].

      . What are the indices of the pairs that are already in the right order?
        (The first pair has index 1, the second pair has index 2, and so on.)

      . Determine which pairs of packets are already in the right order.
        What is the sum of the indices of those pairs?

      . Using the example (input-13-test.txt), the pairs in the right order are:
        1, 2, 4, and 6;
        the sum of these indices is 13.
-}

data PacketVals = OpenBracket | CloseBracket | Val Int
newtype Packet  = Packet [PacketVals]
data Pairs      = Pairs Int (Packet, Packet) deriving Show

data PacketVals' = Val' Int | Nested [PacketVals'] deriving Show
type PacketList' = [PacketVals']
data Pairs'      = Pairs' Int (PacketList', PacketList') deriving Show

instance Show PacketVals where
  show :: PacketVals -> String
  show OpenBracket  = "["
  show CloseBracket = "]"
  show (Val i) = show i

instance Show Packet where
  show :: Packet -> String
  show (Packet ps) =   unpack
                     $ replace findB replB
                     $ replace findA replA (pack $ go ps)
    where
      go [p]    = show p
      go (p:ps) = show p ++ show ps
      findA = pack "[,"
      replA = pack "["
      findB = pack ",]"
      replB = pack "]"

parsePacket' :: String -> PacketList'
parsePacket' [] = []
parsePacket' ('[':cs) =
  let (nested, rest) = span (/= ']') cs
  in  Nested (parsePacket' nested) : parsePacket' rest
parsePacket' (c:cs) | isDigit c =
  let (num, rest) = span isDigit (c:cs)
  in  Val' (read num) : parsePacket' rest
parsePacket' (_:cs) = parsePacket' cs

parsePacket :: String -> [PacketVals]
parsePacket [] = []
parsePacket (c:cs) = case c of
  '[' -> OpenBracket  : parsePacket cs
  ']' -> CloseBracket : parsePacket cs
  ',' ->                parsePacket cs
  _   -> if isDigit c
           then Val (read (c:takeWhile isDigit cs)) : parsePacket (dropWhile isDigit cs)
           else error $ "Unexpected Char: '" ++ [c] ++ "'"

parseInput :: String -> [Pairs]
parseInput input = go 1 lns
  where
    lns = lines input
    go :: Int -> [String] -> [Pairs]
    go _ [] = []
    go cnt ("":rest) = go cnt rest
    go cnt (l:r:rest) = Pairs cnt (Packet parseL, Packet parseR):go (cnt+1) rest
      where
        parseL = parsePacket l
        parseR = parsePacket r

disp :: Show a => [a] -> IO ()
disp pairs = putStrLn $ unlines $ map show pairs

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

cmpPairs :: Pairs -> Int
cmpPairs (Pairs pNum (Packet [], Packet [])) = pNum
cmpPairs (Pairs pNum (Packet (Val lVal:lVals), Packet (Val rVal:rVals))) =
  case compare lVal rVal of
    LT -> pNum
    GT -> 0
    EQ -> cmpPairs (Pairs pNum (Packet lVals, Packet rVals))

cmpPairs (Pairs pNum (Packet (CloseBracket:_), Packet (Val rVal:_))) = pNum
cmpPairs (Pairs pNum (Packet (Val lVal:_), Packet (CloseBracket:_))) = 0

cmpPairs (Pairs pNum (Packet (OpenBracket:lVals), Packet (OpenBracket:rVals))) =
  cmpPairs (Pairs pNum (Packet lVals, Packet rVals))

cmpPairs _ = -1 -- error "Not implemented yet!!"

main :: IO ()
main = do
  fileInput <- readFile "input-13.test" -- "input-13.txt"

  putStrLn "Day 13 - Part A"
  putStrLn fileInput

  hr

  disp $ parseInput fileInput
