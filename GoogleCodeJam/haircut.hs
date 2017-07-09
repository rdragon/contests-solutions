-- 2016-07-10
{-# LANGUAGE ViewPatterns #-}

import Data.List
import Control.Monad

-- ws: waits, d: delta

type M = Int -- minutes
type C = Int -- cut
type W = Int -- wait

main :: IO ()
main = do
  test_c <- fmap read getLine :: IO Int
  forM_ [1..test_c] $ \test_n -> do
    [cuts_c, pred -> nIn] <- fmap (map read . words) getLine :: IO [Int]
    cuts <- fmap (map read . words) getLine :: IO [Int]
    let
      f :: Int -> [W] -> Int
      f n ws = case takeWhile (\(_, m) -> pred m < n) $ map calc $ iterate (* 2) 1 of
        [] -> ((minimum ws) `elemIndices` ws) !! n
        (last -> (ws1, m)) -> f (n - m) ws1
        where
        calc :: M -> ([W], Int)
        calc d = fmap sum $ unzip $ map (run d) $ zip cuts ws
        run :: M -> (C, W) -> (Int, W)
        run d (c, w) = if d > w then (w1, m) else (w - d, 0) where
          m = (d - w + c - 1) `quot` c
          w1 = (w - d) `mod` c
    putStrLn $ "Case #" ++ show test_n ++ ": " ++ show (succ $ f nIn (replicate cuts_c 0))
