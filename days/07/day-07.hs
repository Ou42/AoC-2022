module Main where

import Data.List (break, sort)
-- import Debug.Trace (trace)
import qualified Data.Map.Lazy as Map
import Data.Map.Lazy (Map)
import Data.Maybe (fromJust)

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

      Part B
        . The total disk space available to the filesystem is 70 000 000.
          To run the update, you need unused space of at least 30 000 000.
          You need to find a directory you can delete that will free up
          enough space to run the update.
        . Find the smallest directory that, if deleted, would free up
          enough space on the filesystem to run the update. What is the
          total size of that directory?
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
  -- Mimics "tree" cmd. Skips files ( folders only ).
  let dirItems :: FSZipper -> [FSItem]
      dirItems (Folder _ _ dirItems', _) = dirItems'

      isDir :: FSItem -> Bool
      isDir (Folder _ _ _) = True
      isDir _              = False

      dirsOnly :: [FSItem] -> [FSItem]
      dirsOnly = filter isDir

      subDirNames :: FSZipper -> [Name]
      subDirNames fsz' = map (\(Folder name _ _) -> name) $ dirsOnly $ dirItems fsz'

      dirSize :: FSZipper -> Int
      dirSize fsz' = sum $ map getSize $ dirItems fsz'

      getSize :: FSItem -> Int
      getSize (Folder _ size _) = size
      getSize (File _ size) = size

      go :: [String] -> FSZipper -> [Name] -> [String] -> [String]
      go accu _ [] _          = accu
      -- 2 reasons for moving Up (cd ..)
      -- (1) we're at a leaf node / there are no more subdirs
      -- (2) we've exhausted all the subdirs in the current dir
      -- yet, *both* can be determined by checking if the dir (from dir:dirs) != subdir of curr dir ???
      go accu fsz@(Folder currDir _ items, bs) (dir:dirs) prefix =
        let sdNs = subDirNames (fsTo dir fsz)
        in  -- if length dirs > 25
              -- then  error $ "dirs > 25 |\n  accu = " ++ (unlines accu) ++ "\n  dirs = " ++ (unlines dirs)
              -- else
            if dir `elem` (subDirNames fsz)
              then
                    -- "cd <dir>"
                    go (accu ++ [(concat prefix) ++ dir]) (fsTo dir fsz) (sdNs ++ dirs) ("|  ":prefix)
              else
                    -- "cd .."

                    -- if null items
                    --   then  -- @ leaf node, update folder size
                    --         error "null items" -- never gets called

{-
                    ******************************************************************
                    *                                                                *
                    *   There are no null Folders! Apparently, all folders contain   *
                    *   at least one file!                                           *
                    *                                                                *
                    *   Cannot "update" `accu` as we might be returning from sub-    *
                    *   dir to a folder that was added a while ago!                  *
                    *                                                                *
                    *   ... IOW, we've added other subdirs before returning to the   *
                    *   current one and the one we're editing isn't the curr one!    *
                    *                                                                *
                    ******************************************************************
-}

                    let size = dirSize fsz
                        initAccu = init accu
                        lastAccu = (last accu) ++ " (" ++ (show size) ++ ")"
                        -- lastAccu = (last accu) --  ++ " (" ++ show size ++ ")")
                        newAccu  = initAccu ++ [lastAccu]
                        newFsz  = (Folder currDir size items, bs)
                    in go (newAccu) (fsUp newFsz) (dir:dirs) (tail prefix)
                    --   else
                    --         let size = dirSize fsz
                    --             initAccu = init accu
                    --             -- lastAccu = (last accu ++ " (" ++ show size ++ ")")
                    --             lastAccu = (last accu) --  ++ " (" ++ show size ++ ")")
                    --             newAccu  = initAccu ++ [lastAccu]
                    --         in go (newAccu) (fsUp fsz) (dir:dirs) (tail prefix)

  in unlines $ go [] fszipper (subDirNames fszipper) ["+-- "]

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

parseTermHistory :: String -> FSZipper
parseTermHistory termHist = foldl (flip parseTermLine) emptyFSZipper
                            $ map words $ lines termHist

type Path = [String]
type Dir  = String

-- partAtake2 :: String -> [Int]
-- partAtake2 termHist =
  -- let wrdsPerLine = map words $ lines termHist
{-      
      ... keep track of "base path" & "curr dir" then "curr path == base + curr"
      ... probably use a Map [String] Int to start then see if a List is possible
      ... key *CAN* be a [String] (List of Strings) ... idea being the curr path "backwards"
      ... so maybe a foldl ?
      ... if not reading ahead:
        ... if "<size> <fileName>"  -> update currDir's size
        ... if "cd .."              -> update basePath's size & pop basePath
        ... if "cd <dirName>"       -> update path (including "/")
        ... if "dir <dirName>"      -> create new node
        ... otherwise ignore it ?! ( ignore "$ ls" !! )
      ---
      ... could also keep accumulator summing dirs <= 100K
-}

-- parse :: [String] -> (Path, Dir, Map Path Int) -> (Path, Dir, Map Path Int)

-- parse :: [String] -> (Path, Map Path Int) -> String
parse :: [String] -> (Path, Map Path Int) -> (Path, Map Path Int)
parse ("$":"cd":[dir]) (path, totals) =
  case dir of
    ".."  -> -- "adjust/alter (+) curr dir's size into parent dir's size, move up | " ++ show (path, totals)
              let newPath = tail path
                  currDirSize = fromJust (Map.lookup path totals)
                  newTotals = Map.adjust (currDirSize +) newPath totals
              in (newPath, newTotals)
    _     -> -- "change path | " ++ show (path, totals)
              (dir:path, totals)

parse ("$":_) (path, totals) = (path, totals) -- show (path, totals) -- for "$ ls" or any other "$" cmd

parse ("dir":[dir]) (path, totals) =
  -- "add: (\"" ++ dir ++ "\":path) to tree ... | "  ++ show (path, totals)
  (path, Map.insert (dir:path) 0 totals)

parse (sizeStr:[file]) (path, totals) =
  -- "adjust/alter (+) curr dir's size by " ++ size ++ " | " ++ show (path, totals)
  let sizeInt = read sizeStr :: Int
      newTotals = Map.adjust (sizeInt +) path totals
  in (path, newTotals)

backToRoot :: (Path, Map Path Int) -> Map Path Int
backToRoot (["/"], totals) = totals
backToRoot (path, totals) = backToRoot $ parse ["$", "cd", ".."] (path, totals)

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
  -- putStrLn $ show $ root

  putStrLn $ replicate 42 '-'

  -- DFS output WIP: Directory structure only ( no files )
  -- putStrLn $ dfsFSZipper root
  putStrLn $ replicate 42 '-'

  let cmdLineTerms = map words $ "dir /":(lines f)
  let dirSizes = backToRoot $ foldl (flip parse) ([], Map.empty) cmdLineTerms

  -- Part A

  let totals =  sum
                $ Map.elems
                $ Map.filter (<= 100000)
                $ dirSizes

  putStrLn $ "The answer to Day-07 Part A is: \n\t" ++ show totals -- == 1770595
  
  -- Part B
  
  let dirSizesTotals =  sort $ Map.elems $ dirSizes
  let dirSizesTotal  = fromJust $ Map.lookup ["/"] dirSizes
  let amtReqToFree   =  30 * 10^6 - (70 * 10^6 - dirSizesTotal)
  --                       70 000 000 == 70 * 10^6 ( max fs size )
  --                       30 000 000 == 30 * 10^6 ( min free space req )
  let candidates     = filter (>= amtReqToFree) dirSizesTotals

  putStrLn $  "The answer to Day-07 Part B is: \n\t"
              -- ++ show dirSizes ++ "\n\t"
              ++ show dirSizesTotals ++ "\n\t"
              ++ show dirSizesTotal ++ "\n\t"
              ++ show amtReqToFree ++ "\n\t"
              ++ show candidates ++ "\n\t"
              ++ show (head candidates)
