-- 2017-04-08
-- bathroom-stalls, code jam 2017
-- compiled with GHC 8.0.1
import Control.Monad
import Text.Printf
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \(x, y) ->
      putStrLn $ printf "Case #%d: %d %d" i x y

solve :: IO (Integer, Integer)
solve = do
  [n, k] <- map read. words <$> getLine
  return $ go k (Map.singleton n 1)
  where
  go k dict
    | k <= k' = (n1, n2)
    | otherwise = go (k - k') $ add n1 $ add n2 dict'
    where
    ((n, k'), dict') = Map.deleteFindMax dict
    (n1, n2) = split $ n - 1
    add n' = Map.insertWith (+) n' k'
  split n = let (q, r) = n `quotRem` 2 in (q + r, q)
