-- 2016-05-16
module Main where

import Data.List
import Data.Function

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      _ <- getLine
      as <- fmap (map read . words) getLine :: IO [Int]
      bs <- fmap (map read . words) getLine :: IO [Int]
      let xs = zip as bs
      let ys = reverse $ sortBy (compare `on` (\(x, y) -> x + y)) xs
      let z = sum $ zipWith (\i (x, y) -> if odd i then x else -y) [(1 :: Int)..] ys
      putStrLn (if z == 0 then "Tie" else (if z > 0 then "First" else "Second"))
        
      