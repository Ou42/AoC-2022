import Data.ByteString as B
import Data.Set as S
import Prelude as P

type Items = Set Char

rsContents :: String -> [(Items, Items)]
rsContents f =
  let halfLen = (flip div 2) . P.length
  in P.map ( (\(f,s) -> (S.fromList f, S.fromList s))
           . (\sc -> P.splitAt (halfLen sc) sc)
           ) $ P.lines f

day03PartA sacks =
  P.map ((S.toList) . (\(f,s) -> S.intersection f s)) sacks

main = do
  f <- P.readFile "input-03.txt"
  let sacks = rsContents f

  P.putStrLn $ show $ day03PartA sacks