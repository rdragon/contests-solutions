-- 2016-05-03
import Data.List
import Data.Char

f :: [Int] -> Int
f [] = 0
f (0:1:0:xs) = 1 + f (1:xs)
f (x:xs) = f xs

main = do
  getLine
  xs <- fmap (fmap digitToInt) getLine
  print (f xs)