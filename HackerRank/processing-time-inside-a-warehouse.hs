-- 2016-06-05
import Data.List

main :: IO ()
main = do
  [n, _] <- fmap (map read . words) getLine :: IO [Int]
  ps <- fmap (map read . words) getLine :: IO [Int]
  print (run n ps)
run :: Int -> [Int] -> Int
run n ps = f 1 tmax
  where
    n1 = n * 2
    pmax = maximum ps
    tmax = pmax * n
    f a b
      | a == b = a
      | n1 < x = f a c
      | x < n1 = f (succ c) b
      | otherwise = c
      where
        c = (b - a) `quot` 2 + a
        (q, r) = getValue c
        x = 2 * q + r
    getValue t = foldl' h (0, 1) (map g ps)
      where
        g p = (q, r)
          where
            (q, r) = t `quotRem` p
        h (q, r) (q1, r1) = (q + q1, min r r1)