-- 2016-08-21
import Data.List
import Control.Monad

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [c, _, v] <- fmap (map read . words) getLine :: IO [Int]
    ds <- fmap (map read . words) getLine :: IO [Int]
    let ans = solve c v ds
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans

solve :: Int -> Int -> [Int] -> Int
solve c v dsIn = f 0 dsIn
  where
  f w _
    | w >= v = 0
  f w ds
    | not (null ds) && d <= w' = f (d * c + w) (tail ds)
    | otherwise = 1 + f (w' * c + w) ds
    where
    w' = succ w
    d = head ds
