module Main where

import Data.Char (isDigit)
import Data.Text (pack, replace, unpack)
import Data.Text.Internal.Fusion (stream)

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


parsePacket :: String -> PacketList
parsePacket (_:rest) = Packet (go rest)
  where
    go :: String -> [PacketVals]
    go [] = []
    go ('[':cs) =
      let (nested, rest) = span (/= ']') cs
      in  Nested (go nested) : go rest
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs

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

parseP2 :: String -> PacketList
parseP2 (_:rest) = Packet (go $ init rest)
  where
    go :: String -> [PacketVals]
    go [] = []
    -- go (']':cs) = [] : go cs
    go ('[':cs) =
      let (nested, rest) = span (/= ']') cs
      in  if null rest then error $ ">>>>> emtpy list?!\n"
                                    ++ "nested: " ++ show nested ++ "\n"
                                    ++ "rest: " ++ show rest ++ "\n"
                       else Nested (go nested) : go (tail rest)
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs

parseP2b :: IO ()
parseP2b = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    -- packetStr = "[[[]]]" -- works!
    packetStr = "[[[6,10],[4,3,[4]]]]"
    -- packetForm = Packet (go packetStr [])
    packetForm = Packet [Nested $ go packetStr]
    -- packetForm = Packet (go (tail packetStr) [])
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals]
    go [] = []
    -- go (']':cs) = [] : go cs
    go ('[':cs) =
      let (nested, rest) = span (/= ']') cs
      in  Nested (go nested) : go rest
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs

parseP2c :: IO ()
parseP2c = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    -- packetStr = "[[[]]]" -- works!
    packetStr  = "[[[6,10],[4,3,[4]]]]"
    innerNested [Nested packetVals] = packetVals
    packetForm = Packet $ innerNested $ go packetStr
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals]
    go [] = []
    go ('[':cs) =
      -- let (nested, rest) = span (/= ']') cs
      let (nested, rest) = splitP ('[':cs)
      in  Nested (go $ tail nested) : go rest
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs

-- parseP3 :: (String, String, Bool) -- String -> PacketList
parseP3 :: IO ()
parseP3 = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    -- packetStr = "[[[]]]" -- works!
    packetStr = "[[[6,10],[4,3,[4]]]]"
    -- packetForm = Packet (go packetStr [])
    packetForm = Packet [Nested $ go packetStr]
    -- packetForm = Packet (go (tail packetStr) [])
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals]
    go [] = []
    go (']':c:cs) = case c of
                      ',' -> go cs
                      _   -> go cs
    go ('[':cs) = go cs
    go (c:cs) | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  Val (read num) : go rest
    go (_:cs) = go cs 
    -- go todo acc = error $ ">>>> More to do?!\n"
    --                       ++ "left to do: " ++ show todo ++ "\n"
    --                       ++ "acc: " ++ show acc ++ "\n"


parseP4 :: IO ()
parseP4 = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    packetStr = "[[[6,10],[4,3,[4]]]]"
    packetForm = Packet [Nested $ go packetStr []]
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals] -> [PacketVals]
    go [] [] = []
    go (']':c:cs) acc = case c of
                          ',' -> Nested acc : go cs []
                          _ -> [Nested acc]
    go ('[':cs) acc = acc ++ go cs []
    go (c:cs) acc | isDigit c =
      let (num, rest) = span isDigit (c:cs)
      in  go rest (acc ++ [Val (read num)])
    go (_:cs) acc = go cs acc

bingParse :: IO ()
bingParse = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    packetStr = "[[[6,10],[4,3,[4]]]]"
    packetForm = Packet (go packetStr [])
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals] -> [PacketVals]
    go [] acc = reverse acc
    go (x:xs) acc
      | x == '['  = go xs (Nested (go xs []) : acc)
      | x == ']'  = acc
      | isDigit x = let (num, rest) = span isDigit (x:xs)
                    in  go rest (Val (read num) : acc)
      | otherwise = go xs acc

bingParse2 :: IO ()
bingParse2 = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    packetStr = "[[[6,10],[4,3,[4]]]]"
    packetForm = Packet (go packetStr [])
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals] -> [PacketVals]
    go [] acc = reverse acc
    go (x:xs) acc
      | x == '['  = let (nested, rest) = span (/= ']') xs
                    in  go rest (Nested (go nested []) : acc)
      | x == ']'  = acc
      | isDigit x = let (num, rest) = span isDigit (x:xs)
                    in  go (dropWhile (== ',') rest) (Val (read num) : acc)
      | otherwise = go xs acc

bingParse3 :: IO ()
bingParse3 = putStrLn $ packetStr ++ " source\n"
                     ++ res ++ " converted/reverted\n"
                     ++ show packetForm ++ " packet format\n"
                     ++ "Match: " ++ show isCorrect
  where
    packetStr = "[[[6,10],[4,3,[4]]]]"
    packetForm = Packet (go packetStr [])
    res = dispPacket packetForm
    isCorrect = packetStr == res
    go :: String -> [PacketVals] -> [PacketVals]
    go [] acc = reverse acc
    go (x:xs) acc
      | x == '['  = let (nested, rest) = span (/= ']') xs
                    in  go rest (Nested (go nested []) : acc)
      | x == ']'  = acc
      | isDigit x = let (num, rest) = span isDigit (x:xs)
                    in  go (dropWhile (== ',') rest) (Val (read num) : acc)
      | otherwise = go xs acc

parseInput :: String -> Pairs
parseInput input = go 1 lns
  where
    lns = lines input
    go :: Int -> [String] -> Pairs
    go _ [] = []
    go cnt ("":rest) = go cnt rest
    go cnt (l:r:rest) = Pair cnt (parseL, parseR):go (cnt+1) rest
      where
        parseL = parsePacket l
        parseR = parsePacket r

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

cmpPair :: Pair -> Int
cmpPair (Pair pNum (Packet lPacket, Packet rPacket)) = go lPacket rPacket
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
    -- go lVals@(Nested nLeft:_) (Val rVal:rVals)  = go lVals (Nested [Val rVal]:rVals)
    -- go (Val lVal:lVals) rVals@(Nested nRight:_) = go (Nested [Val lVal]:lVals) rVals

    -- Pattern match is redundant?!
    -- go l r = error $ "BLAH BLAH BLAH BLAH BLAH"
    --                  ++ "\n"
    --                  ++ "Left: " ++ show l
    --                  ++ "\n"
    --                  ++ "Right: " ++ show r

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

  disp $ take 5 $ parseInput fileInput

  hr

  print $ map cmpPair $ parseInput fileInput

  hr

  putStr "Sum = "
  print $ sum $ map cmpPair $ parseInput fileInput

  -- using my personalized input data, I end up w/ 5486
  -- > That's not the right answer; your answer is too high.
