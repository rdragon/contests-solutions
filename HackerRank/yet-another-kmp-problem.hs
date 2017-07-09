-- 2016-05-19
module Main where

import Data.List
import Data.Char

main :: IO ()
main = do
  cs <- fmap (map read . words) getLine :: IO [Int]
  let is = run cs
  putStrLn $ [chr (ord 'a' + i) | i <- is]

run :: [Int] -> [Int]
run cs
  | all (== 0) cs = []
  | length nz_is == 1 = replicate (sum cs) (head nz_is)
  | mc <= 2 || not mc_i_smallest = mc_i : getLexico 0 (cs1 ++ [pred mc] ++ cs2)
  | otherwise = mc_i : mc_i : myIntersperse (mc - 2) is1
  where
    mc = minimum $ filter (> 0) cs
    (Just mc_i) = mc `elemIndex` cs
    mc_i_smallest = not $ any (> 0) (take mc_i cs)
    nz_is = findIndices (> 0) cs
    getLexico _ [] = []
    getLexico i (c:cs3) = replicate c i ++ getLexico (succ i) cs3
    (cs1, (_:cs2)) = splitAt mc_i cs
    myIntersperse 0 is = is
    myIntersperse n (i:is) = i:mc_i:myIntersperse (pred n) is
    myIntersperse _ _ = undefined
    is1 = getLexico 0 (cs1 ++ [0] ++ cs2)

    
    