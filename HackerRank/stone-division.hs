-- 2016-11-03
-- stone-division, week of code 25
import qualified Data.IntMap.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Applicative

main :: IO ()
main = do
  [n, _] <- fmap (map read . words) getLine
  ss <- fmap (map read . words) getLine
  putStrLn $ if solve n ss then "First" else "Second"

solve :: Int -> [Int] -> Bool
solve n ss
  | any (\s -> even s && n `mod` s == 0) ss = True
  | otherwise = evalState (calc n ss) (Map.empty)

calc :: Int -> [Int] -> State (Map.IntMap Bool) Bool
calc n ss = do
  mp <- get
  case Map.lookup n mp of
    Just x -> return x
    Nothing -> do
      x <- or . map not <$> sequence [calc q ss | s <- ss, let (q, r) = n `quotRem` s, r == 0]
      modify $ Map.insert n x
      return x
