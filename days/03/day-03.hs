import qualified Data.List as L
import Data.List.Split (chunksOf)
import qualified Data.Set as S

{-
    https://adventofcode.com/2022/day/3

    Given a list of contents in backpacks...

        (A) . split the contents in half
            . find the duplicate item in each half
            . convert the item to a value
              [a..z] == [1..26]
              [A..Z] == [27..52]
            . return sum of all values

        (B) . group every 3 rucksacks
            . find the one duplicated item in each
              of the 3 rucksacks
            . convert item to a value as above
            . return sum of all values
-}

type Items = S.Set Char

rsContents :: String -> [(Items, Items)]
rsContents f =
  let halfLen = (flip div 2) . length
  in map ( (\(f,s) -> (S.fromList f, S.fromList s))
           . (\sc -> splitAt (halfLen sc) sc)
           ) $ lines f

day03PartA :: [(Items, Items)] -> Maybe Int
day03PartA sacks =
  let lu = ' ' : ['a'..'z'] <> ['A'..'Z'] -- 'a' is at index 1 ...
  in
    fmap sum $ sequence $
    map ((flip L.elemIndex lu) 
          . head 
          . (S.toList)
          . (\(f,s) -> S.intersection f s)) sacks

rsContentsPartB :: String -> [[Items]]
rsContentsPartB f =
  chunksOf 3 $ map S.fromList $ lines f

day03PartB :: [[Items]] -> Maybe Int
day03PartB sacks =
  let lu = ' ' : ['a'..'z'] <> ['A'..'Z'] -- 'a' is at index 1 ...
  in
    fmap sum $ sequence $
    map ((flip L.elemIndex lu) 
         . head
         . (S.toList)
         . (\[s1, s2, s3] -> S.intersection s1 (S.intersection s2 s3))) sacks


main = do
  f <- readFile "input-03.txt"
  let sacks = rsContents f

  putStrLn $ "Part A = " <> show (day03PartA sacks)

  let sacksPartB = rsContentsPartB f
  putStrLn $ "Part B = " <> show (day03PartB sacksPartB)
