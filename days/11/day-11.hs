{-# LANGUAGE MultiWayIf #-}

module Main where

import Control.Applicative ((<|>))
import Data.Char ( isDigit )
import Data.List ( sort )
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
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
-}

data Monkey = Monkey {
    mID :: Int
  , items :: [Int]
  , op :: Int -> Int
  , ifT :: Int
  , ifF :: Int
}

showMonkey :: Monkey -> String
showMonkey m = show $ mID m

monkeyInLine :: String -> Bool
monkeyInLine line = "Monkey" `elem` words line

getMonkeyID :: String -> Int
getMonkeyID line = read $ init (words line !! 1)

startsWithStarting :: String -> Bool
startsWithStarting line = "Starting" == head (words line)

monkeyStrings :: String -> [[String]]
monkeyStrings str =
  let lns = lines str
  in
    foldl (\ms ln -> (if ln == "" then []:ms else (head ms ++ [ln]):tail ms) ) [[]] lns

-- happyPathParser :: String -> ???
happyPathParser str =
  let -- lns = lines str
      ms = monkeyStrings str
      blankMnky = Monkey (-1) [] (+0) (-1) (-1)
  in
    map ( foldl (\mnky ln -> if
              | (monkeyInLine ln) -> mnky { mID = getMonkeyID ln }
              -- | 
              | otherwise -> mnky ) blankMnky ) ms

------------------------------------------------------------

type MonkeyKey = Int

-- Parser Combinator version
data ReadPMonkey = ReadPMonkey {
    rpID    :: Int
  , rpItems :: [Int]
  , rpOp    :: Int -> Int
  , rpTest  :: Int -> Bool
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

parseCSV :: ReadP [Int]
parseCSV = commaSep parseInt

parseCommaSpcSV :: ReadP [Int]
parseCommaSpcSV = commaSpcSep parseInt

---

--- Bing-Chat suggested solution for parsing an "operation" func

parseVar :: ReadP String
parseVar = many1 (satisfy (`elem` ['a'..'z']))

parseNum :: ReadP Int
parseNum = read <$> many1 (satisfy (`elem` ['0'..'9']))

parseOp :: ReadP (Int -> Int -> Int)
parseOp =  (char '+' >> return (+))
       <|> (char '-' >> return (-))
       <|> (char '*' >> return (*))
       <|> (char '/' >> return div)

parseExpr :: ReadP (Int -> Int)
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

parseFunc :: String -> Maybe (Int -> Int)
parseFunc str = case readP_to_S parseExpr str of
    [(f, "")] -> Just f
    _         -> Nothing

-- parseTest :: ReadP (Int -> Int)
parseTest :: ReadP (Int -> Bool)
parseTest = do
  skipSpaces
  string "Test: divisible by "
  num <- parseNum
  satisfy (== '\n')

  return ( \x -> (x `rem` num) == 0)

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

readPmonkeyItems :: ReadP [Int]
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

doOneOp :: Map Int ReadPMonkey -> MonkeyKey -> Map Int ReadPMonkey
doOneOp monkeysMap monkeyKey =
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

doOneRound :: Map Int ReadPMonkey -> Map Int ReadPMonkey
doOneRound monkeysMap = foldl doOneOp monkeysMap $ M.keys monkeysMap

do_20_Rounds :: Map Int ReadPMonkey -> Map Int ReadPMonkey
do_20_Rounds monkeyMap = foldl (\monkeyMap' i -> doOneRound monkeyMap') monkeyMap [1..20]

partA :: Map Int ReadPMonkey -> Int
partA monkeyMap =
  -- find the top 2 Inspections
  -- multiply them together

  let [firstMax, secondMax] = take 2 
                              $ reverse
                              $ sort
                              $ M.foldr ((:) . rpInspected) [] $ do_20_Rounds monkeyMap
  in
     firstMax * secondMax


main :: IO ()
main = do
  -- fileInput <- readFile "input-11-test.txt"
  fileInput <- readFile "input-11.txt"

  putStrLn $ replicate 42 '-'

  let msMap = makeMonkeysMap fileInput

  putStrLn "before:"
  print msMap

  putStrLn $ replicate 42 '-'
  putStrLn "after one operation:"
  print $ doOneOp msMap 0

  putStrLn $ replicate 42 '-'
  putStrLn "Part A -- answer:"
  print $ partA msMap
