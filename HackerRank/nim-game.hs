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
      as <- fmap (map read . words) getLine :: IO [Int]
      putStrLn (if foldl' xor 0 as /= 0 then "First" else "Second")
  
    
  