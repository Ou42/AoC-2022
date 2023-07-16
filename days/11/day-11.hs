{-# LANGUAGE MultiWayIf #-}

module Main where

import qualified Data.Map as M
import Text.ParserCombinators.ReadP

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

readPmonkeyID :: ReadP Int
readPmonkeyID = do
    string "Monkey "
    mID <- fmap read $ many1 (satisfy (\char -> char >= '0' && char <= '9'))
    satisfy (== ':')
    return mID

-- metar :: ReadP Report
-- metar = do
--     code <- airport
--     time <- timestamp
--     wind <- windInfo
--     return (Report code time wind)

-- and this actually works!

-- In[43]:

-- λ> readP_to_S metar "BIRK 281500Z 09014G17KT CAVOK M03/M06 Q0980 R13/910195"

readPMonkeyData :: ReadP ReadPMonkey
readPMonkeyData = do
    id <- readPmonkeyID
    return (ReadPMonkey id [42, 42])

main :: IO ()
main = do
  f <- readFile "input-11-test.txt"
  -- f <- readFile "input-11.txt"

  putStrLn $ replicate 42 '-'

  let parsedInput = happyPathParser f

  -- putStrLn $ unlines $ map showMonkey $ take 3 parsedInput
  print "42"

  print $ readP_to_S readPMonkeyData f