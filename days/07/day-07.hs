module Main where

import Data.List (break)  

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

data FSCrumb = FSCrumb Name Size [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper  
fsUp (item, FSCrumb name size ls rs:bs) = (Folder name size (ls ++ [item] ++ rs), bs)

fsToRoot :: FSZipper -> FSZipper
fsToRoot fszipper@(Folder "/" _ _, _) = fszipper
fsToRoot fszipper = fsUp fszipper

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName size items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName size ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _ _) = name == folderName
nameIs name (File fileName _) = name == fileName

dfsFSZipper :: FSZipper -> String
dfsFSZipper fszipper =
  let dirItems :: FSZipper -> [FSItem]
      dirItems (Folder _ _ dirItems', _) = dirItems'
      isDir :: FSItem -> Bool
      isDir (Folder _ _ _) = True
      isDir _              = False
      dirsOnly :: [FSItem] -> [FSItem]
      dirsOnly = filter isDir
      subDirNames :: FSZipper -> [Name]
      subDirNames fsz' = map (\(Folder name _ _) -> name) $ dirsOnly $ dirItems fsz'
      go :: [String] -> FSZipper -> [[Name]] -> [String]
      go accu _ []            = accu
      -- go accu fsz@(Folder folderName _ _,_) ((dir:dirs):todo) = go (dir:accu) fsz (dirs:todo)

      go accu fsz ([]:todo) = error "dir:dirs == []" -- go accu fsz (todo)
      go accu fsz ((dir:dirs):todo) = go (dir:accu) (fsTo dir fsz) ((subDirNames fsz):dirs:todo)
      -- go accu fsz ([]:todo) = accu -- go (dir:accu) fsz (dirs:todo)
      
  -- in go "" fszipper firstDirItems
  -- in unlines (subDirNames firstDirDirs)
  -- in unlines $ go [] fszipper [(subDirNames firstDirDirs)]
  in unlines $ go [] fszipper [(subDirNames fszipper)]

fsNewItem :: FSItem -> FSZipper -> FSZipper
fsNewItem item (Folder folderName size items, bs) =
    (Folder folderName size (item:items), bs)

emptyFSZipper :: FSZipper
emptyFSZipper = (Folder "" (-1) [], [])

parseTermLine :: [[Char]] -> FSZipper -> FSZipper
parseTermLine ("$":"cd":["/"])  _        = (Folder "/" (-1) [], [])
parseTermLine ("$":"cd":[".."]) fszipper = fsUp fszipper
parseTermLine ("$":"cd":[dir])  fszipper = fsTo dir fszipper

parseTermLine ("$":["ls"])     fszipper = fszipper
parseTermLine ("$":_)          fszipper = error "unexpected data"
parseTermLine ("dir":[dir])    fszipper = fsNewItem (Folder dir (-1) []) fszipper
parseTermLine (size:[file])    fszipper = fsNewItem (File file (read size :: Int)) fszipper
parseTermLine _                fszipper = error "unexpected data"

-- parseTermHistory
parseTermHistory termHist = foldl (flip parseTermLine) emptyFSZipper
                            $ map words $ lines termHist
-- parseTermHistory termHist = map ((flip parseTermLine (,)) . words) $ lines termHist

main :: IO ()
main = do
  f <- readFile "input-07.txt"

  -- looked to see if fs changes ... inconclusive
  -- Data.List.sort $ filter (not . flip (elem) ["cd","$","ls","dir",".."]) $ concatMap words $ lines f
  --    . did see dupe files/folders (e.g. "ttntt"), but are they in the same sub-dir?

  -- parsing appears to be working
  -- *Main> a = unlines $ take 15 $ lines f
  -- *Main> a
  -- "$ cd /\n$ ls\ndir brhvclj\ndir clnvqg\ndir dtqtvvrn\ndir lcz\ndir pcqjncwl\ndir qwvfpgl\ndir rtmj\ndir shg\ndir tcdmgwp\n$ cd brhvclj\n$ ls\n40016 mtlscfrd.gdr\ndir mvslzl\n"
  -- *Main> parseTermHistory a
  -- (Folder "brhvclj" (-1) [Folder "mvslzl" (-1) [],File "mtlscfrd.gdr" 40016],[FSCrumb "/" (-1) [Folder "tcdmgwp" (-1) [],Folder "shg" (-1) [],Folder "rtmj" (-1) [],Folder "qwvfpgl" (-1) [],Folder "pcqjncwl" (-1) [],Folder "lcz" (-1) [],Folder "dtqtvvrn" (-1) [],Folder "clnvqg" (-1) []] []])

  putStrLn $ replicate 42 '-'

  let root = fsToRoot $ parseTermHistory f
  putStrLn $ show $ root

  putStrLn $ replicate 42 '-'

  -- DFS output WIP
  putStrLn $ dfsFSZipper root
  putStrLn $ replicate 42 '-'
