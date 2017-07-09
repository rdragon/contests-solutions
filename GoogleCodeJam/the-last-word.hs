-- 2016-06-15
import Control.Monad

main :: IO ()
main = do
  n <- fmap read getLine :: IO Int
  forM_ [1..n] $ \i -> do
    s <- getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ solve s []
  where
    solve :: String -> String -> String
    solve [] t = t
    solve (x:s) [] = solve s [x]
    solve (x:s) t@(y:_) = solve s (if x < y then (t ++ [x]) else (x:t))