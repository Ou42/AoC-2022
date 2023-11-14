module Main where

import Data.Char (isDigit)

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

data PacketVals    = Val Int | Nested [PacketVals] deriving Show
newtype PacketList = Packet [PacketVals] deriving Show
data Pair          = Pair Int (PacketList, PacketList) deriving Show
type Pairs         = [Pair]

splitP :: String -> (String, String)
splitP str = go ([head str], tail str)
  where
    go (p1,[]) = (p1,[])
    go (p1,p2) =
      let p1OpenBracketCnt  = length $ filter (=='[') p1
          p1CloseBracketCnt = length $ filter (==']') p1
      in  if p1OpenBracketCnt == p1CloseBracketCnt
            then (p1, p2)
            else go (p1 ++ [head p2], tail p2)

parseP2c2PacketValsList :: String -> [PacketVals]
parseP2c2PacketValsList [] = []
parseP2c2PacketValsList ('[':cs) =
  let (nested, rest) = splitP ('[':cs)
  in  Nested (parseP2c2PacketValsList $ tail nested) : parseP2c2PacketValsList rest
parseP2c2PacketValsList (c:cs) | isDigit c =
  let (num, rest) = span isDigit (c:cs)
  in  Val (read num) : parseP2c2PacketValsList rest
parseP2c2PacketValsList (_:cs) = parseP2c2PacketValsList cs

parseP2c :: String -> PacketList
parseP2c packetStr =
  let innerNested [Nested packetVals] = packetVals
  in  Packet $ innerNested $ parseP2c2PacketValsList packetStr

parseP2cTest_1 :: IO ()
parseP2cTest_1 = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    -- packetStr = "[[[]]]" -- works!
    packetStr  = "[[[6,10],[4,3,[4]]]]"
    packetForm = parseP2c packetStr
    res = dispPacket packetForm
    isCorrect = packetStr == res

parseInput :: String -> Pairs
parseInput input = go 1 lns
  where
    lns = lines input
    go :: Int -> [String] -> Pairs
    go _ [] = []
    go cnt ("":rest) = go cnt rest
    go cnt (l:r:rest) = Pair cnt (parseL, parseR):go (cnt+1) rest
      where
        parseL = parseP2c l
        parseR = parseP2c r

disp :: Show a => [a] -> IO ()
disp pairs = putStrLn $ unlines $ map show pairs

disp2 :: Pairs -> IO ()
disp2 pairs = putStrLn $ unlines $ map dispPair pairs

dispPair :: Pair -> String
dispPair (Pair pNum (pLeft, pRight)) = "Pair " ++ show pNum ++ ": "
                                        ++ dispPacket pLeft ++ ", " ++ dispPacket pRight

dispPacket :: PacketList -> String
dispPacket (Packet pLst) = "[" ++ go pLst ++ "]"
  where
    go :: [PacketVals] -> String
    go [] = ""
    go [Val v] = show v
    go (Val v:vals)  = show v ++ "," ++ go vals
    go [Nested vs]   = "[" ++ go vs ++ "]"
    go (Nested v:vs) = "[" ++ go v ++ "]," ++ go vs

hr :: IO ()
hr = putStrLn $ replicate 42 '-' ++ ['\n']

cmpPairPartA :: Pair -> Int
cmpPairPartA (Pair pNum (Packet lPacket, Packet rPacket)) = go lPacket rPacket
  where
    go :: [PacketVals] -> [PacketVals] -> Int
    go [] [] = -1 -- inconclusive, continue

    -- compare 2 Ints
    go (Val lVal:lVals) (Val rVal:rVals) =
      case compare lVal rVal of
        LT -> pNum
        GT -> 0
        EQ -> go lVals rVals

    -- if Left is exhausted first, the inputs are in the right order
    go [] (Val rVal:_) = pNum
    -- if Right is exhausted first, the inputs are NOT in the right order
    go (Val lVal:_) [] = 0

    -- compare 2 Nested Lists
    go (Nested nLeft:lVals) (Nested nRight:rVals) =
      let res = go nLeft nRight
      in case res of
           -1 -> go lVals rVals
           _  -> res

    -- if Left is exhausted first, the inputs are in the right order
    go [] (Nested rVal:_) = pNum
    -- if Right is exhausted first, the inputs are NOT in the right order
    go (Nested lVal:_) [] = 0

    -- if only one of the 2 is an Int
    -- ... convert the Int to a Nested List and re-compare
    go lVals (Val rVal:rVals) = go lVals (Nested [Val rVal]:rVals)
    go (Val lVal:lVals) rVals = go (Nested [Val lVal]:lVals) rVals

prettyPrintInputData :: String -> IO ()
prettyPrintInputData input = putStrLn $ go 1 lns
  where
    lns = lines input
    go :: Int -> [String] -> String
    go _ [] = []
    go cnt ("":rest) = go cnt rest
    go cnt (l:r:rest) = "Pair " ++ show cnt ++ ": " ++ l ++ ", " ++ r
                        ++ "\n"
                        ++ go (cnt+1) rest


main :: IO ()
main = do
  -- fileInput <- readFile "input-13.test"
  fileInput <- readFile "input-13.txt"

  putStrLn "Day 13 - Part A"
  putStrLn fileInput

  hr

  let packetPairList = parseInput fileInput
  disp $ take 5 packetPairList

  hr

  let validPairs = map cmpPairPartA packetPairList
  print validPairs

  hr

  putStr "Sum = "
  print $ sum validPairs
