import qualified Data.List as L
import qualified Data.Set as S

type ElfPos = (Char, Int)

elfPosFromFile :: String -> [[ElfPos]]
elfPosFromFile f =
  map ((filter ((=='#'). fst))
      . flip zip [0..])
      $ lines f

main = do
  f <- readFile "input-23-test.txt"

  putStrLn "-- raw elf position data:"
  putStrLn f

  let es = elfPosFromFile f
  putStrLn "-- WIP parse of elf position data:"
  putStrLn $ unlines $ map show es

-- []
-- []
-- [('#',7)]
-- [('#',5),('#',6),('#',7),('#',9)]
-- [('#',3),('#',7),('#',9)]
-- [('#',4),('#',8),('#',9)]
-- [('#',3),('#',5),('#',6),('#',7)]
-- [('#',3),('#',4),('#',6),('#',8),('#',9)]
-- [('#',4),('#',7)]
-- []
-- []
-- []
