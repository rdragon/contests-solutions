-- 2016-06-20
import Control.Monad

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    s <- getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve s)
solve :: String -> Int
solve s0 = let (xs, n) = f s0 in 10 * n + (length xs `quot` 2) * 5 where
  f [] = ([], 0)
  f (x:xs)
    | null ys || head ys /= x = (x:ys, n)
    | otherwise = (tail ys, succ n)
    where
    (ys, n) = f xs
