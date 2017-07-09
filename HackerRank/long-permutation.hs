-- 2016-05-19
module Main where

import qualified Data.Vector.Unboxed as U
import Control.Monad.ST
import qualified Data.Vector.Unboxed.Mutable as M

main :: IO ()
main = do
  [_, m] <- fmap (map (pred . read) . words) getLine :: IO [Int]
  p <- fmap (map (pred . read) . words) getLine :: IO [Int]
  print (succ $ run p m)
run :: [Int] -> Int -> Int
run ps m
  | m >= pred a = pred n + m - pred a
  | otherwise = go m (p 0)
  where
    pv = U.fromList ps
    n = length ps
    a = steps (pred n)
    steps i
      | j == 0 = 1
      | otherwise = succ $ steps (pred j)
      where j = q i
    p = (pv U.!)
    q = (qv U.!)
    qv = runST $ do
      v <- M.new n
      sequence_ [M.write v j i | (i, j) <- zip [0..] ps]
      U.freeze v
    go k i
      | k == 0 || i == pred n = k + i
      | otherwise = go (pred k) (p $ succ i)