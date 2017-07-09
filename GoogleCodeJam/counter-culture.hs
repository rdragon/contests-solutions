-- 2016-08-19
import Control.Monad

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [n] <- fmap (map read . words) getLine :: IO [Int]
    let ans = solve n
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans

solve :: Int -> Int
solve n
  | n <= 9 = n
  | n `mod` 10 == 0 = 1 + solve (pred n)
  | otherwise = extra + read t2 + read (reverse t1) + solve (read $ replicate (pred m) '9')
  where
  s = show n
  m = length s
  (t1, t2) = splitAt (m `quot` 2) s
  extra = if head t1 == '1' && all (== '0') (tail t1) then 0 else 1
  