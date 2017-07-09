-- 2016-09-15
-- unexpected problem, week of code 23
import qualified Data.Vector as V

main :: IO ()
main = do
  v <- fmap V.fromList getLine
  m <- fmap read getLine
  print $ m `quot` block v
  
block :: V.Vector Char -> Int
block v = go
  where
  l = V.length v
  at = (v V.!)
  w = V.generate l f
  kmp = (w V.!)
  f 0 = 0
  f n = g n $ kmp $ pred n
  g n k
    | at n == at k = succ k
    | k == 0 = 0
    | otherwise = g n $ kmp $ pred k
  go
    | a * 2 < l = l
    | l `mod` b == 0 = b
    | otherwise = l
    where
    a = kmp $ pred l
    b = l - a
    