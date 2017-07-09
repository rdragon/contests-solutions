-- 2017-04-08
-- oversized-pancake-flipper, code jam 2017
-- compiled with GHC 8.0.1
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \m ->
      putStrLn $ printf "Case #%d: %s" i $ maybe "IMPOSSIBLE" show m

solve :: IO (Maybe Int)
solve = (\[s, k] -> go (read k) 0 s). words <$> getLine
  where
  go k !n s
    | all (== '+') s = Just n
    | length s < k = Nothing
    | head s == '+' = go k n $ tail s
    | otherwise = go k (n + 1) $ map swap s1 ++ s2
    where (s1, s2) = splitAt k s
  swap '-' = '+'
  swap _ = '-'
