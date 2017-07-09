-- 2016-06-16
import Data.List
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    [n, r, p, s] <- fmap (map read . words) getLine :: IO [Int]
    putStrLn $ "Case #" ++ show i ++ ": " ++ solve n [r, p, s] where
  solve n rps = fromMaybe "IMPOSSIBLE" . listToMaybe . map fromJust . filter isJust . map (solve' n rps) . nub $ permutations [1, 0, 0] where
  solve' n rps rps0
    | rps /= play n rps0 = Nothing
    | otherwise = Just $ f n ("RPS" !! fromJust (1 `elemIndex` rps0)) where
      f 0 c = [c]
      f m 'R' = g m 'R' 'S'
      f m 'P' = g m 'P' 'R'
      f m 'S' = g m 'S' 'P'
      f _ _ = undefined
      g m c d = concat . sort $ [f (pred m) c, f (pred m) d]
  play 0 rps = rps
  play n [r, p, s] = play (pred n) [r + p, p + s, s + r]
  play _ _ = undefined
