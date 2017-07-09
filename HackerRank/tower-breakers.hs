-- 2016-05-14
module Main where

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      [n, m] <- fmap (map read . words) getLine :: IO [Int]
      print (if m == 1 then 2 else (if n `mod` 2 == 0 then 2 else 1))
  
    
  