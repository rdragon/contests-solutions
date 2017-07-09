-- 2016-05-14
module Main where

import Data.Bits
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      _ <- getLine
      hs <- fmap (map read . words) getLine :: IO [Int]
      putStrLn (if foldl' xor (0 :: Int) (map divs hs) /= 0 then "1" else "2")
  
primes :: [Int]    
primes = 2:3:5:[p | p <- [7, 9..], and [p `mod` q /= 0 | q <- takeWhile (\z -> z * z <= p) primes]]

divs :: Int -> Int
divs 1 = 0
divs m = f primes m
  where
    f _ 1 = 0
    f (p:ps) n
      | n `mod` p == 0 = 1 + f (p:ps) (n `div` p)
      | otherwise = f ps n
    f _ _ = undefined
  