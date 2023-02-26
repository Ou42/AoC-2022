module Main where

{-
    Day 07

      Part A
        . given commands ( `cd`, `ls` ) to navigate a filesystem
          & resulting output
              . commands are preceded by "$ "
              . `cd <dir-name>` moves to the sub-dir <dir-name>
              . `cd ..` moves to the parent directory
              . "/" is the root folder
              . `ls` lists the contents of the current directory
                  . "dir <dir-name>"
                  . "<file-size> <file-name>"
        . Find all of the directories with a total size of at
          most 100K, then calculate the sum of their total sizes.
              . directories include sub-directory totals
-}

-- from LYAH:
--   http://www.learnyouahaskell.com/zippers
--   "A very simple file system"

type Name = String  
type Size = Int  
data FSItem = File Name Size | Folder Name Size [FSItem] deriving (Show)

-- parseTermLine parts
parseTermLine ("$":"cd":[dir]) = "command = cd to " ++ dir
parseTermLine ("$":["ls"])     = "command = ls"
parseTermLine ("dir":[dir])    = "dir = " ++ dir
parseTermLine (size:[file])    = "file = " ++ file ++ " size = " ++ size
parseTermLine _                = error "unexpected data"

-- parseTermHistory
-- parseTermHistory = foldl ...
parseTermHistory termHist = map (parseTermLine . words) $ lines termHist

main :: IO ()
main = do
  f <- readFile "input-07.txt"

  putStrLn $ replicate 42 '-'
  putStrLn $ show $ Folder "dir 01" 0 [File "yes" 42]
  putStrLn $ replicate 42 '-'
