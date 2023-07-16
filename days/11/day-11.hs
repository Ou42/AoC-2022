{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Data.Char

{-
    Day 11

      Part A - Monkey Business
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

-- Parser Combinator version
data ReadPMonkey = ReadPMonkey {
    rpmID :: Int
  , rpitems :: [Int]
} deriving (Show)

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
    id <- readPmonkeyID
    items <- readPmonkeyItems
    return (ReadPMonkey id items)

main :: IO ()
main = do
  f <- readFile "input-11-test.txt"
  -- f <- readFile "input-11.txt"

  putStrLn $ replicate 42 '-'

  let parsedInput = happyPathParser f

  -- putStrLn $ unlines $ map showMonkey $ take 3 parsedInput
  print "42"

  print $ readP_to_S readPMonkeyData f