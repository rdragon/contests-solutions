-- 2016-05-18
module Main where

import Data.List

main :: IO ()
main = do
  n <- fmap read getLine :: IO Int
  ls <- fmap (map read . words) getLine :: IO [Int]
  putStrLn $ calc n (reverse $ sort ls)

calc :: Int -> [Int] -> String
calc n ls
  | null xs = "-1"
  | otherwise = show a' ++ " " ++ show b' ++ " " ++ show c'
  where
    xs = [[a, b, c] | i <- [0..n - 1], j <- [i + 1..n - 1], k <- [i + 1, i + 2.. j - 1], [a, b, c] <- permutations [ls !! i, ls !! j, ls !! k], abs (a - b) < c && c < a + b]
    [a', b', c'] = sort (head xs)