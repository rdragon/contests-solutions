-- 2017-04-30
-- cruise-control, code jam 2017
-- compiled with GHC 8.0.1
import Control.Monad
import Text.Printf

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \x ->
      putStrLn $ printf "Case #%d: %f" i x

solve :: IO Double
solve = do
  [d', n] <- fmap (map read. words) getLine :: IO [Int]
  let d = fromIntegral d'
  t <- fmap maximum $ replicateM n $ fmap (f d. map read. words) getLine :: IO Double
  return $ d / t
  where
  f d [l, s] = (d - l) / s
  f _ _ = undefined
