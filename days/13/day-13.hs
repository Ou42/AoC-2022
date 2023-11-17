module Main where

import Data.Char (isDigit)
import Data.List (foldl')

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

parseP2c :: String -> PacketList
parseP2c = Packet . innerNested . go
  where
    innerNested [Nested packetVals] = packetVals
    go :: String -> [PacketVals]
    go [] = []
    go ('[':cs) =
      let (nested, rest) = splitP ('[':cs)
      in  Nested (go $ tail nested) : go rest
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs


-- data PacketVals    = Val Int | Nested [PacketVals] deriving Show
parseFoldP :: String -> PacketList
parseFoldP packetStr =
  let (Nested packetVals) = foldP packetStr
  in  Packet packetVals

-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldP :: String -> PacketVals
-- foldP = head . foldl' go2 ([]::[PacketVals])
foldP = head . snd . foldl' go3 ('!',[]::[PacketVals])

go2 :: [PacketVals] -> Char -> [PacketVals]
go2 (Nested pv:pvs) c | isDigit c = Nested (pv ++ [Val (read [c])]) : pvs
go2 pvs '['  = Nested [] : pvs
go2 [pv] ']' = [pv]
go2 (pv1 : Nested pv2 : pvs) ']' = Nested (pv2 ++ [pv1]) : pvs
go2 pvs ','  = pvs -- default action will be append
go2 pvs c    = error $ "--- char: " ++ [c] ++ " ---- is invalid! ----\n"

go3 :: (Char, [PacketVals]) -> Char -> (Char, [PacketVals])
go3 (prevC, Nested pv:pvs) c | isDigit c =
  if isDigit prevC
    then
      let Val prevDigit = last pv
      in  (c, Nested ( init pv ++ [Val (prevDigit*10 + read [c])]) : pvs)
    else
      (c, Nested (pv ++ [Val (read [c])]) : pvs)
go3 (_,  pvs) '[' = ('!', Nested [] : pvs)
go3 (_, [pv]) ']' = ('!', [pv])
go3 (_, pv1 : Nested pv2 : pvs) ']' = ('!', Nested (pv2 ++ [pv1]) : pvs)
go3 (_,  pvs) ',' = ('!', pvs) -- default action will be append
go3 (_,  pvs) c   = error $ "--- char: " ++ [c] ++ " ---- is invalid! ----\n"

go :: (String, [[PacketVals]]) -> (String, [[PacketVals]])
go ([],vals) = ([],vals)

go (']':cs, vals) = go (cs,vals)
                    -- error $ "--- Whaaaaaaaat! ----------------------------" ++ "\n"
                    -- ++ "cs: " ++ cs ++ "\n"
                    -- ++ "vals: " ++ show vals ++ "\n"

go ('[':cs, vals) = undefined -- (cs, [Nested $ snd $ go (cs, vals)-+]:vals)

go (',':cs, v:vals) = (cs, v : head (snd (go (cs, vals))) : vals)

go (c:cs, vals) | isDigit c =
                              let (num, rest) = span isDigit (c:cs)
                              in  go (rest, [Val (read num)] : vals)
                | otherwise =
                              error $ "-----\n"
                                    ++ "c: " ++ [c] ++ "\n"

parseP3RecursiveOnly :: String -> PacketList
parseP3RecursiveOnly str = Packet $ innerNested $ head $ snd $ go (tail str, [[]])
  where
    innerNested [Nested packetVals] = packetVals
    innerNested huh = error $ show huh
    go :: (String, [[PacketVals]]) -> (String, [[PacketVals]])
    go ([],vals) = ([],vals)
    -- go (']':cs, [v]) = go (cs,[v])
    -- go (']':cs, [v1]:[v2]:vals) = go (cs,[Nested [v2, v1]]:tail vals)
    go (']':cs, vals) = -- go (cs,vals)
                        error $ "--- Whaaaaaaaat! ----------------------------" ++ "\n"
                        ++ "cs: " ++ cs ++ "\n"
                        ++ "vals: " ++ show vals ++ "\n"
    -- go ('[':cs, vals) = (cs, [Nested $ head (snd $ go (cs,vals))]:vals)
    go ('[':cs, vals) = (cs, [Nested $ head (snd (go (cs, vals))) ]:vals)
    -- go ('2':cs, vals) = error $ "--- Whaaaaaaaat! ----------------------------" ++ "\n"
    --                           ++ "cs: " ++ cs ++ "\n"
    --                           ++ "vals: " ++ show vals ++ "\n"
    go (',':cs, v:vals) = (cs, v : head (snd (go (cs, vals))) : vals)
        -- error $ "------- , ---------------------------" ++ "\n"
        -- ++ "cs: " ++ cs ++ "\n"
        -- ++ "v:vals: " ++ show (v:vals) ++ "\n" -- (cs, v : head (snd (go (cs, vals))) : vals)

    go (c:cs, vals) | isDigit c =
                                  let (num, rest) = span isDigit (c:cs)
                                  in  go (rest, [Val (read num)] : vals)
                    | otherwise =
                                  error $ "-----\n"
                                        ++ "c: " ++ [c] ++ "\n"

    -- go (cs, vals) =  

{-
ghci> parseFuncTest parseP3RecursiveOnly "[1]"
[1] source
[[],1] converted/reverted
Packet [Nested [],Val 1] packet format
Match: False
-}

parseFuncTest :: (String -> PacketList) -> String -> IO ()
parseFuncTest parseFunc packetStr = putStrLn $ packetStr ++ " source\n"
                                    ++ res ++ " converted/reverted\n"
                                    ++ show packetForm ++ " packet format\n"
                                    ++ "Match: " ++ show isCorrect
  where
    packetForm = parseFunc packetStr
    res = dispPacket packetForm
    isCorrect = packetStr == res

parseP2cTest_1 :: IO ()
parseP2cTest_1 = parseFuncTest parseP2c "[[[6,10],[4,3,[4]]]]"
-- parseP2cTest_1 = parseFuncTest parseP2c "[[[]]]" -- works!

parseInput :: String -> (String -> PacketList) -> Pairs
parseInput input parseFunc = go 1 lns
  where
    lns = lines input
    go :: Int -> [String] -> Pairs
    go _ [] = []
    go cnt ("":rest) = go cnt rest
    go cnt (l:r:rest) = Pair cnt (parseL, parseR):go (cnt+1) rest
      where
        parseL = parseFunc l
        parseR = parseFunc r

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
  fileInput <- readFile "input-13.test"
  -- fileInput <- readFile "input-13.txt"

  putStrLn "Day 13 - Part A"
  putStrLn fileInput

  hr

  let packetPairList = parseInput fileInput parseP2c
  disp $ take 5 packetPairList

  hr

  let validPairs = map cmpPairPartA packetPairList
  print validPairs

  hr

  putStr "Sum ( using `parseP2c` ) = "
  print $ sum validPairs

  hr

  let packetsFromFold    = parseInput fileInput parseFoldP
  let validPairsFromFold = map cmpPairPartA packetsFromFold

  putStr "Sum ( using `parseFoldP` ) = "
  print $ sum validPairsFromFold
