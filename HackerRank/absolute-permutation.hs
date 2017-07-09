-- 2016-05-22
import Data.List

main :: IO ()
main = do
  [t] <- fmap (map read . words) getLine
  sequence_ $ replicate t run
  where
    run = do
      [n, k] <- fmap (map read . words) getLine
      putStrLn (calc n k)
    calc n 0 = pretty [1..n]
    calc n k
      | n `mod` (k * 2) == 0 = pretty . concat $ [map (+(i*k*2)) ([k+1..2*k] ++ [1..k]) | i <- [0..n `quot` (k * 2)-1]]
      | otherwise = "-1"
    pretty :: [Int] -> String
    pretty xs = intercalate " " (map show xs)

