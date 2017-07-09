-- 2016-05-15
module Main where

import qualified Data.Vector as V
import Data.List
import Data.Bits

type Vec = V.Vector

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      k <- fmap read getLine :: IO Int
      xs <- fmap (map (map read . words)) $ sequence $ replicate k getLine :: IO [[Int]]
      let ys = [get x y | [x, y] <- xs]
      putStrLn (if foldl' xor 0 ys /= 0 then "First" else "Second")
  
gs :: Vec Int
gs = V.generate 225 f
  where
    f m = head [n | n <- [0..], n /= a, n /= b, n /= c, n /= d]
      where
        (y0, x0) = m `divMod` 15
        x = x0 + 1
        y = y0 + 1
        a = get (x - 2) (y - 1)
        b = get (x - 2) (y + 1)
        c = get (x - 1) (y - 2)
        d = get (x + 1) (y - 2)

index :: Int -> Int -> Int
index x y = (y - 1) * 15 + x - 1
        
get :: Int -> Int -> Int
get x y
  | x <= 0 || y <= 0 || x > 15 || y > 15 = -1
  | otherwise = gs V.! (index x y)
  
  