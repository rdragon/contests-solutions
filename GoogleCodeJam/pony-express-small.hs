-- 2017-04-30
-- pony-express-small, code jam 2017
-- compiled with GHC 8.0.1
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Text.Printf
import qualified Data.Vector as V

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \x ->
      putStrLn $ printf "Case #%d: %f" i x

solve :: IO Double
solve = do
  [n, _] <- fmap (map read. words) getLine :: IO [Int]
  esv <- fmap V.fromList $ replicateM n $ fmap ((\[x, y] -> (x, y)). map read. words) getLine :: IO (V.Vector (Double, Double))
  dv <- fmap V.fromList $ replicateM n getRow :: IO (V.Vector (V.Vector Double))
  let ds = [(dv V.! i) V.! succ i | i <- [0..n-1]]
  _ <- getLine
  return $ rec esv ((fst $ esv V.! 0, 0) : replicate (n - 1) (-1, inf)) ds n 1
  where
  getRow = fmap (V.fromList. map read. words) getLine
  rec esv !ets (d:ds) n i
    | i == n = minimum $ map snd ets
    | otherwise = rec esv (go n esv ets i d) ds n (i + 1)
  go n esv ets i d = [calc h | h <- [0..n-1]]
    where
    best_t = minimum [f e t h | ((e, t), h) <- zip ets [0..]]
    calc h
      | h == i = (e_h, best_t)
      | e' >= -eps = (e', t + d / s)
      | otherwise = (-1, inf)
      where
      (e, t) = ets !! h
      (e_h, s) = esv V.! h
      e' = e - d
    f e t h
      | e' >= -eps = t + d / s
      | otherwise = inf
      where
      e' = e - d
      s = snd $ esv V.! h
  eps = 0.000000001
  inf = 1 / 0
