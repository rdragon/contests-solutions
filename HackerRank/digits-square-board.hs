-- 2016-05-16
module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
-- import qualified Data.Vector.Unboxed.Mutable as M
import Data.List
-- import Control.Monad.ST
import Data.Bits
import Data.Int

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      n <- fmap read getLine :: IO Int
      ds <- fmap (map ((\x -> if x `elem` "14689" then 1 else 0) . head) . words . intercalate " ") (sequence $ replicate n getLine)
      let cs = U.fromList $ getCounts n ds
      putStrLn (calc n cs)

getCounts :: Int -> [Int] -> [Int]
getCounts n ds = cs
  where
    cs = zipWith (+) (replicate n 0 ++ cs) (f ds)
    f [] = []
    f xs = scanl1 (+) xs1 ++ f xs2 where (xs1, xs2) = splitAt n xs

calc :: Int -> U.Vector Int -> String
calc n cs = if compute 0 0 (n - 1) (n - 1) /= 0 then "First" else "Second"
  where
    mem = V.generate (n * n * n * n) f :: V.Vector Int8
    f i
      | x2 == x1 && y2 == y1 = 0
      | count == 0 = 0
      | otherwise = mex $ [compute x1 y1 x y2 `xor` compute (succ x) y1 x2 y2 | x <- [x1 .. pred x2]] ++ [compute x1 y1 x2 y `xor` compute x1 (succ y) x2 y2 | y <- [y1 .. pred y2]] 
      where
        (x1:y1:x2:y2:_) = map (`mod` n) (iterate (`div` n) i)
        count = getC x2 y2 + getC (x1 - 1) (y1 - 1) - getC x2 (y1 - 1) - getC (x1 - 1) y2
    getC x y
      | x < 0 || y < 0 = 0
      | otherwise = cs U.! (x + n * y)
    compute :: Int -> Int -> Int -> Int -> Int8
    -- compute x1 y1 x2 y2 = f (x1 + n * y1 + n * n * x2 + n * n * n * y2)
    compute x1 y1 x2 y2 = mem V.! (x1 + n * y1 + n * n * x2 + n * n * n * y2)
    mex xs = x where (x, _) = head . filter (\(y, z) -> y /= z) $ zip [0..] ((++ [-1]) . map head . group . sort $ xs)
      