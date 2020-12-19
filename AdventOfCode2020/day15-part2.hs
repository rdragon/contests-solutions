-- This solution has complexity n * log(n).
-- It takes almost one minute on my machine.
-- I guess I'm missing something.

import qualified Data.Map.Strict as M

main = do
    xs <- fmap (map read . words . map (\c -> if c == ',' then ' ' else c)) getContents
    print . f (M.fromList $ zip (init xs) [1..]) (length xs) $ last xs

f :: M.Map Int Int -> Int -> Int -> Int
f _ 30000000 n = n
f ts t n = n' `seq` f ts' (succ t) n'
    where mt' = M.lookup n ts
          n' = case mt' of Just t' -> t - t'
                           Nothing -> 0
          ts' = M.insert n t ts