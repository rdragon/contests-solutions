-- 2016-09-13
-- lighthouse, week of code 23
import Data.List
import qualified Data.Vector as V
import Control.Monad

main = do
  n <- fmap read getLine :: IO Int
  v <- fmap (V.fromList . concat) $ replicateM n getLine
  let
    bound = maximum [minimum $ map (^ (2 :: Int)) [succ i, succ j, n - i, n - j] ++ f i j | i <- [0..pred n], j <- [0..pred n]]
    f i0 j0 = [dx * dx + dy * dy | i <- [0..pred n], j <- [0..pred n], v V.! (i * n + j) == '*', let dx = abs (i - i0), let dy = abs (j - j0)]
  print $ (subtract 1) $ head $ dropWhile (\r -> r * r < bound) [1..]
  