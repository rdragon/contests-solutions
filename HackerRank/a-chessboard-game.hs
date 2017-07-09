-- 2016-05-13
module Main where

import qualified Data.Vector as V
type Vec = V.Vector

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      [x, y] <- fmap (map read . words) getLine :: IO [Int]
      putStrLn (if get x y then "First" else "Second")
  
as :: Vec Bool
as = V.generate 225 f
  where
    f n = not (a && b && c && d)
      where
        (y0, x0) = n `divMod` 15
        x = x0 + 1
        y = y0 + 1
        a = get (x - 2) (y - 1)
        b = get (x - 2) (y + 1)
        c = get (x - 1) (y - 2)
        d = get (x + 1) (y - 2)

index :: Int -> Int -> Int
index x y = (y - 1) * 15 + x - 1
        
get :: Int -> Int -> Bool
get x y
  | x <= 0 || y <= 0 || x > 15 || y > 15 = True
  | otherwise = as V.! (index x y)