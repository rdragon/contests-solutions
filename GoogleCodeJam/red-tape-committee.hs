-- 2016-06-23
-- analyse gelezen
import Data.List
import Control.Monad

main :: IO ()
main = do
  testc <- fmap read getLine :: IO Int
  forM_ [1..testc] $ \testn -> do
    [_, k] <- fmap (map read . words) getLine :: IO [Int]
    ps <- fmap (sort . map read . words) getLine :: IO [Double]
    putStrLn $ "Case #" ++ show testn ++ ": " ++ show (solve ps k)

solve :: [Double] -> Int -> Double
solve ps k
  | n == k = calc k ps
  | otherwise = maximum . map (calc k . take k) . take (succ k) . tails $ qs
  where
  n = length ps
  qs = (reverse . take k $ ps) ++ (reverse . drop (n - k) $ ps)

calc :: Int -> [Double] -> Double
calc k ps' = last (f ps')
  where
  l = k `quot` 2
  f [] = 1 : replicate l 0
  f (p:ps) = let qs = f ps in zipWith (+) (map (* (1 - p)) qs) (0 : map (* p) qs)
