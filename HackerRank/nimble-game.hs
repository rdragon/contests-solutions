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
      cs <- fmap (map read . words) getLine :: IO [Int]
      putStrLn (if foldl' xor 0 [i | (i, c) <- zip [(0 :: Int) ..] cs, c `mod` 2 == 1] /= 0 then "First" else "Second")
  
    
  