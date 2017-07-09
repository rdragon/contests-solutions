-- 2016-05-18
module Main where

import Data.List
import Data.Bits

main :: IO ()
main = do
  n <- fmap read getLine :: IO Int
  as <- fmap (map read . words) getLine :: IO [Int]
  print $ calc n as

calc :: Int -> [Int] -> Int
calc n as
  | null xs || null ys = 0
  | otherwise = ans
  where
    bs = remove as
    c = maximum [maxPower 1 b | b <- bs]
    (xs, ys) = partition (\z -> ((z .&. c) == c)) bs
    ans = minimum [x `xor` y | x <- xs, y <- ys]

remove :: [Int] -> [Int]
remove as@(a0:_)
  | and [(a .&. b) == b && a - b < b | a <- as] = remove [a - b | a <- as]
  | otherwise = as
  where
    b = maxPower 1 a0

maxPower :: Int -> Int -> Int
maxPower x 0 = x
maxPower x 1 = x
maxPower x y = maxPower (x * 2) (y `quot` 2)