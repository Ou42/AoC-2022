{-# LANGUAGE StrictData #-}

module Main where

import Control.Applicative ((<|>))
import Data.Char ( isDigit )
import Data.List ( foldl', sort )
import Data.Map.Strict (Map) -- Data.Map (Map)
import qualified Data.Map.Strict as M -- Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP
    ( char,
      eof,
      many1,
      readP_to_S,
      satisfy,
      sepBy,
      skipSpaces,
      string,
      ReadP )

{-
    Day 11

      Part A - Monkey Business
        . a group of monkeys have our stuff (items)
        . they throw items between themselves based on some rules
        . keep track of which monkey has what item based on a worry factor

        . focus on the two most active monkeys if you want any hope
          of getting your stuff back. Count the total number of times
          each monkey inspects items over 20 rounds

        . what is the result of multiplying the number of items inspected
          by the two most active monkeys?

      Part B -
        . this time, worry doesn't go down! there is no (`div` 3)
        . also, run for 10,000 rounds

        . what is the result of multiplying the number of items inspected
          by the two most active monkeys?

-}

type MonkeyKey = Int

-- Parser Combinator version
data ReadPMonkey = ReadPMonkey {
    rpID    :: Int
  , rpItems :: [Integer] -- [Int]
  , rpOp    :: Integer -> Integer
  , rpTest  :: Integer -> Bool
  , rpIfT   :: Int
  , rpIfF   :: Int
  , rpInspected :: Int
}

instance Show ReadPMonkey where
  show :: ReadPMonkey -> String
  show r = "ReadPMonkey { id = "
            ++ show (rpID r) ++ ", items = " ++ show (rpItems r)
            ++ ", rpOp = <function>"
            ++ ", rpTest = <function>"
            ++ ", rpInspected = " ++ show (rpInspected r) ++ "}\n"

-- instance Show [ReadPMonkey] where
--   show = map show

--- slightly tweaked Bing-Chat suggested solution to reading a CSV list of Ints

commaSep :: ReadP a -> ReadP [a]
commaSep p = p `sepBy` char ','

commaSpcSep :: ReadP a -> ReadP [a]
commaSpcSep p = p `sepBy` string ", "

parseInt :: ReadP Int
parseInt = read <$> many1 (satisfy isDigit)

parseInteger :: ReadP Integer
parseInteger = read <$> many1 (satisfy isDigit)

parseCSV :: ReadP [Int]
parseCSV = commaSep parseInt

-- parseCommaSpcSV :: ReadP [Int]
parseCommaSpcSV :: ReadP [Integer]
parseCommaSpcSV = commaSpcSep parseInteger -- parseInt

---

--- Bing-Chat suggested solution for parsing an "operation" func

parseVar :: ReadP String
parseVar = many1 (satisfy (`elem` ['a'..'z']))

parseNum :: ReadP Int
parseNum = read <$> many1 (satisfy (`elem` ['0'..'9']))

parseOp :: ReadP (Integer -> Integer -> Integer)
parseOp =  (char '+' >> return (+))
       <|> (char '-' >> return (-))
       <|> (char '*' >> return (*))
       <|> (char '/' >> return div)

parseExpr :: ReadP (Integer -> Integer)
parseExpr = do
    skipSpaces
    string "Operation: "
    var1 <- parseVar
    skipSpaces
    char '='
    skipSpaces
    var2 <- parseVar
    skipSpaces
    op <- parseOp
    skipSpaces
    var3 <- parseVar <|> fmap show parseNum
    satisfy (== '\n')

    return (\x -> if var1 == "new" && var2 == "old"
                        then op x (if var3 == "old"
                                       then x
                                       else read var3)
                        else x)

-- parseFunc :: String -> Maybe (Int -> Int)
-- parseFunc str = case readP_to_S parseExpr str of
--     [(f, "")] -> Just f
--     _         -> Nothing

parseTest :: ReadP (Integer -> Bool)
parseTest = do
  skipSpaces
  string "Test: divisible by "
  num <- parseNum
  satisfy (== '\n')

  return ( \x -> (x `rem` toInteger num) == 0)

parseIfTrue :: ReadP Int
parseIfTrue = do
  skipSpaces
  string "If true: throw to monkey "
  trueTo <- parseNum
  satisfy (== '\n')

  return trueTo

parseIfFalse :: ReadP Int
parseIfFalse = do
  skipSpaces
  string "If false: throw to monkey "
  falseTo <- parseNum
  satisfy (== '\n')

  return falseTo

---

readPmonkeyID :: ReadP Int
readPmonkeyID = do
    string "Monkey "
    mID <- parseInt
    satisfy (== ':')
    satisfy (== '\n')
    return mID

-- readPmonkeyItems :: ReadP [Int]
readPmonkeyItems :: ReadP [Integer]
readPmonkeyItems = do
    skipSpaces
    string "Starting items: "
    items <- parseCommaSpcSV
    satisfy (== '\n')
    return items

readPMonkeyData :: ReadP ReadPMonkey
readPMonkeyData = do
    skipSpaces
    id    <- readPmonkeyID
    items <- readPmonkeyItems
    op    <- parseExpr
    test  <- parseTest
    ifT   <- parseIfTrue
    ifF   <- parseIfFalse
    skipSpaces
    return (ReadPMonkey id items op test ifT ifF 0)

readPAllMonkeys :: ReadP [ReadPMonkey]
readPAllMonkeys = many1 readPMonkeyData

makeMonkeysMap :: String -> Map Int ReadPMonkey
makeMonkeysMap f =
  let (lsOfMonkeys, _) = last $ readP_to_S readPAllMonkeys f
  in
      M.fromList $ map (\m -> (rpID m, m)) lsOfMonkeys

doOneOpPartA :: Map Int ReadPMonkey -> MonkeyKey -> Map Int ReadPMonkey
doOneOpPartA monkeysMap monkeyKey =
  -- given a Key, looks up the monkey from the Map
  -- performs operation on all items held by given Monkey
  -- ... and divides by 3 and rounds down
  -- then, based on a Test func, throws item to:
  --        destTrueMonkey
  --     of destFalseMonkey
  -- therefore, no items will remain with the current Monkey
  -- updates the Monkey Map with changes to these 3 Monkeys

  let monkey    = monkeysMap M.! monkeyKey
      operation = rpOp monkey
      items     = map ((`div` 3) . operation) $ rpItems monkey
      test      = rpTest monkey
      (itemsT, itemsF)
                = foldl (\(t, f) i -> if test i
                                        then (t ++ [i], f)
                                        else (t, f ++ [i]))
                        ([],[])
                        items

      destTrueMonkey  = monkeysMap M.! rpIfT monkey
      destFalseMonkey = monkeysMap M.! rpIfF monkey

      newInspectedCnt = rpInspected monkey + length items

      (currMonkeyID, trueMonkeyID, falseMonkeyID)
                = (rpID monkey, rpID destTrueMonkey, rpID destFalseMonkey )

      destTrueItems  = rpItems destTrueMonkey ++ itemsT
      destFalseItems = rpItems destFalseMonkey ++ itemsF

      updates = [ ( currMonkeyID, monkey { rpItems = [], rpInspected = newInspectedCnt })
                , ( trueMonkeyID, destTrueMonkey { rpItems = destTrueItems })
                , ( falseMonkeyID, destFalseMonkey { rpItems = destFalseItems })
                ]
  in
      M.union (M.fromList updates) monkeysMap

doOneOpPartB :: Map Int ReadPMonkey -> MonkeyKey -> Map Int ReadPMonkey
doOneOpPartB monkeysMap monkeyKey =
  -- given a Key, looks up the monkey from the Map
  -- performs operation on all items held by given Monkey
  -- ... and divides by 3 and rounds down
  -- then, based on a Test func, throws item to:
  --        destTrueMonkey
  --     of destFalseMonkey
  -- therefore, no items will remain with the current Monkey
  -- updates the Monkey Map with changes to these 3 Monkeys

  let monkey    = monkeysMap M.! monkeyKey
      operation = rpOp monkey
      -- items     = map operation $ rpItems monkey
      -- itemsCalc = map operation $ rpItems monkey
      itemsCalc = map ((`rem` (13*17*19*23)) . operation) $ rpItems monkey
      items     = if any (> toInteger (maxBound :: Int)) itemsCalc
                    then
                        --   trace ("\t Monkey " ++ show monkeyKey ++ " newItems = " ++ show itemsCalc)
                        --     error $ "oops!\n"
                        --         ++ replicate 75 '-' ++ "\n"
                        --         ++ "items = \t\t" ++ show itemsCalc ++ "\n"
                        --         ++ "maxBound :: Int == \t  " ++ show (maxBound :: Int) ++ "\n"
                        --         ++ replicate 75 '=' ++ "\n"
                        itemsCalc
                   -- else trace ("\t Monkey " ++ show monkeyKey ++ " newItems = " ++ show itemsCalc) itemsCalc
                    else itemsCalc

      test      = rpTest monkey
      (itemsT, itemsF)
                = foldl' (\(t, f) i -> if test i
                                         then (t ++ [i], f)
                                         else (t, f ++ [i]))
                         ([],[])
                         items

      destTrueMonkey  = monkeysMap M.! rpIfT monkey
      destFalseMonkey = monkeysMap M.! rpIfF monkey

      newInspectedCnt = rpInspected monkey + length items

      (currMonkeyID, trueMonkeyID, falseMonkeyID)
                = (rpID monkey, rpID destTrueMonkey, rpID destFalseMonkey )

      destTrueItems  = rpItems destTrueMonkey ++ itemsT
      destFalseItems = rpItems destFalseMonkey ++ itemsF

      updates = [ ( currMonkeyID, monkey { rpItems = [], rpInspected = newInspectedCnt })
                , ( trueMonkeyID, destTrueMonkey { rpItems = destTrueItems })
                , ( falseMonkeyID, destFalseMonkey { rpItems = destFalseItems })
                ]
  in
      if null $ rpItems monkey
        then monkeysMap
        else M.union (M.fromList updates) monkeysMap

doOneRoundPartA :: Map Int ReadPMonkey -> Map Int ReadPMonkey
doOneRoundPartA monkeysMap = foldl doOneOpPartA monkeysMap $ M.keys monkeysMap

doOneRoundPartB :: Map Int ReadPMonkey -> Map Int ReadPMonkey
doOneRoundPartB monkeysMap = foldl' doOneOpPartB monkeysMap $ M.keys monkeysMap

do_20_RoundsPartA :: Map Int ReadPMonkey -> Map Int ReadPMonkey
do_20_RoundsPartA monkeyMap = foldl (\monkeyMap' i -> doOneRoundPartA monkeyMap') monkeyMap [1..20]

do_10K_RoundsPartB :: Map Int ReadPMonkey -> Map Int ReadPMonkey
-- do_10K_RoundsPartB monkeyMap = foldl' (\monkeyMap' i -> doOneRoundPartB monkeyMap') monkeyMap [1..10000]
do_10K_RoundsPartB monkeyMap = 
  -- foldr (\i monkeyMap' -> trace ("calling doOneRoundPartB with Rnd = " ++ show i) doOneRoundPartB monkeyMap') monkeyMap [1..10000]
  foldl' (\monkeyMap' i -> trace ("calling doOneRoundPartB with Rnd = " ++ show i) doOneRoundPartB monkeyMap') monkeyMap [1..10000]

partA :: Map Int ReadPMonkey -> Int
partA monkeyMap =
  -- find the top 2 Inspections
  -- multiply them together

  let [firstMax, secondMax] = take 2
                              $ reverse
                              $ sort
                              $ M.foldr ((:) . rpInspected) [] $ do_20_RoundsPartA monkeyMap
  in
     firstMax * secondMax

partB :: Map Int ReadPMonkey -> Int
partB monkeyMap =
  -- find the top 2 Inspections
  -- multiply them together

  let [firstMax, secondMax] = take 2
                              $ reverse
                              $ sort
                              $ M.foldr ((:) . rpInspected) [] $ do_10K_RoundsPartB monkeyMap
  in
     firstMax * secondMax

main :: IO ()
main = do
  fileInput <- readFile "input-11-test.txt"
  -- fileInput <- readFile "input-11.txt"

  putStrLn $ replicate 42 '-'

  let msMap = makeMonkeysMap fileInput

  putStrLn "before:"
  print msMap

  putStrLn $ replicate 42 '-'
  putStrLn "after one operation:"
  print $ doOneOpPartA msMap 0

  putStrLn $ replicate 42 '-'
  putStrLn "Part A -- answer:"
  print $ partA msMap

  putStrLn $ replicate 42 '-'
  putStrLn "Monkey Map:"
  print msMap

  putStrLn $ replicate 42 '-'
  putStrLn "Part B -- answer:"
  -- print $ "skipped" -- partB msMap
  let partB_test_ans = 2713310158
  let my_partB_ans = partB msMap
  putStrLn $ "my Part B answer = " ++ show my_partB_ans

  -- repeated due to trace output showing before number printed
  putStrLn $ "my Part B answer = " ++ show my_partB_ans

  putStrLn $ "Part B *test* data-set answer = " ++ show partB_test_ans
  putStrLn $ "are they equal? " ++ show (my_partB_ans == partB_test_ans)

  putStrLn $ replicate 42 '-'
  putStrLn "Part B -- test data-set magic number: (13*17*19*23)"
  putStrLn "                                change ^^^^^^^^^^^ for my puzzle input!"
  putStrLn "  (consider parsing the file and extrating and calculating this number?!)"
