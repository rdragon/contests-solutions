-- 2016-05-17
module Main where

import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Data.Bits
import Control.Monad.ST
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      n <- fmap read getLine :: IO Int
      pairs <- sequence . replicate (n - 1) $ fmap (map read . words) getLine :: IO [[Int]]
      putStrLn (calc (getEdges n pairs))
    getEdges n pairs = runST $ do
      e <- M.replicate (n + 1) []
      sequence_ [f e u v | [u, v] <- pairs]
      V.freeze e
      where
        f e u v = do
          M.modify e (u:) v
          M.modify e (v:) u

calc :: V.Vector [Int] -> String
calc e = if f 0 1 == 0 then "Bob" else "Alice"
  where
    f :: Int -> Int -> Int
    f w u = foldl' xor 0 [1 + f u v | v <- e V.! u, v /= w]
