-- 2016-08-21
import Data.List
import Control.Monad

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [r, c, w] <- fmap (map read . words) getLine :: IO [Int]
    let ans = solve r c w
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans

solve r c w = extra + f c
  where
  extra = (r - 1) * (c `quot` w)
  f c'
    | c' == w = w
    | c' < 2 * w = w + 1
    | otherwise = 1 + f (c' - w)
