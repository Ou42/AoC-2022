module Main where

import Data.Char (isDigit)
import Data.List (foldl')
import Data.Semigroup ()

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

instance Semigroup PacketVals where
  (<>) :: PacketVals -> PacketVals -> PacketVals
  Val x <> Val y = Val (x*10 + y)
  Val x <> Nested ys = Nested (Val x : ys)
  Nested xs <> Val y = Nested (xs ++ [Val y])
  Nested xs <> Nested ys = Nested (xs ++ [Nested ys])


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

parseLookAhead :: String -> PacketList
parseLookAhead = Packet . (\[Nested pVals] -> pVals) . go
  where
    go :: String -> [PacketVals]
    go [] = []
    go ('[':cs) =
      let (nested, rest) = splitP ('[':cs)
      in  Nested (go $ tail nested) : go rest
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs


-- foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldP :: ((Char, [PacketVals]) -> Char -> (Char, [PacketVals]))
         -> String -> PacketVals
foldP goFunc = head . snd . foldl' goFunc ('!',[]::[PacketVals])


parseFoldSemigroup :: String -> PacketList
parseFoldSemigroup = Packet . (\(Nested pVals) -> pVals) . foldP go4
  where
    go4 :: (Char, [PacketVals]) -> Char -> (Char, [PacketVals])
    go4 (prevC, pv@(Nested ePV):pvs) c | isDigit c =
      if isDigit prevC
        then
          let -- ePV == extractedPV
              Val prevDigit = last ePV
          in  (c, Nested ( init ePV <> [Val (prevDigit*10 + read [c])]) : pvs)
        else
          (c, (pv <> Val (read [c])) : pvs) -- type checks here!
    go4 (_,  pvs) '[' = ('!', Nested [] : pvs)
    go4 (_, [pv]) ']' = ('!', [pv])
    go4 (_, pv1 : pv2 : pvs) ']' = ('!', (pv2 <> pv1) : pvs)
    go4 (_,  pvs) ',' = ('!', pvs) -- default action will be append
    go4 (_,  pvs) c   = error $ "--- char: " ++ [c] ++ " ---- is invalid! ----\n"


parseFold :: String -> PacketList
parseFold = Packet . (\(Nested pVals) -> pVals) . foldP go3
  where
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


-- Functor instance:

  -- ghci> data Val a = Val a deriving Show
  -- ghci> instance Functor Val where; fmap f (Val x) = Val (f x)

  -- ghci> fmap (*10) $ Val 42
  -- Val 420
  -- ghci> fmap ((+3) . (*10)) $ Val 42
  -- Val 423
  -- ghci> fmap ((+3) . (*10)) $ Val 2
  -- Val 23

-- ----------------------------------------------------------------

parseFuncTest :: (String -> PacketList) -> String -> IO ()
parseFuncTest parseFunc packetStr = putStrLn $ packetStr ++ " source\n"
                                    ++ res ++ " converted/reverted\n"
                                    ++ show packetForm ++ " packet format\n"
                                    ++ "Match: " ++ show isCorrect
  where
    packetForm = parseFunc packetStr
    res = dispPacket packetForm
    isCorrect = packetStr == res

parseLookAheadTest_1 :: IO ()
parseLookAheadTest_1 = parseFuncTest parseLookAhead "[[[6,10],[4,3,[4]]]]"
-- parseLookAheadTest_1 = parseFuncTest parseLookAhead "[[[]]]" -- works!

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

  let packetPairList = parseInput fileInput parseLookAhead
  disp $ take 5 packetPairList

  hr

  let validPairs = map cmpPairPartA packetPairList
  print validPairs

  putStr "Sum ( using `parseLookAhead` ) = "
  print $ sum validPairs

  hr

  let packetsFromFold    = parseInput fileInput parseFold
  let validPairsFromFold = map cmpPairPartA packetsFromFold

  print validPairsFromFold
  putStr "Sum ( using `parseFold` ) = "
  print $ sum validPairsFromFold

  hr

  let packetsFromFold'    = parseInput fileInput parseFoldSemigroup
  let validPairsFromFold' = map cmpPairPartA packetsFromFold'

  print validPairsFromFold'
  putStr "Sum ( using `parseFoldSemigroup` ) = "
  print $ sum validPairsFromFold'
