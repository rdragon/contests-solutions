-- 2017-04-08
-- tidy-numbers, code jam 2017
-- compiled with GHC 8.0.1
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \s ->
      putStrLn $ printf "Case #%d: %s" i $ dropWhile (== '0') s

solve :: IO String
solve = fmap go getLine
  where
  go [] = []
  go s@(c:cs)
    | map (const c) s <= s = c : go cs
    | otherwise = pred c : map (const '9') cs
