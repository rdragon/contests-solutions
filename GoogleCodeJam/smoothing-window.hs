-- 2016-09-07
-- smoothing window, code jam 2015
import Data.Function
import Data.List
import Control.Monad

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rd :: IO [Int]
rd = fmap (map read . words) getLine

solveM :: IO String
solveM = do
  [_, k] <- rd
  sums <- rd
  let
    intervals = getIntervals k sums
    (aligned, cost) = alignIntervals intervals
    final = pushUp aligned ((cost + head sums) `mod` k)
  return $ show $ maximum (map snd final) - minimum (map fst final)

getIntervals :: Int -> [Int] -> [(Int, Int)]
getIntervals k sums = map f $ take k $ tails sums ++ repeat []
  where
  f xs = h $ (0 :) $ scanl1 (+) $ g xs
  g (x:y:xs) = (y - x) : g (drop (k - 2) xs)
  g _ = []
  h xs = (minimum xs, maximum xs)

alignIntervals :: [(Int, Int)] -> ([(Int, Int)], Int)
alignIntervals ps = (qs, sum cs)
  where
  a = minimum $ map fst ps
  (qs, cs) = unzip $ map (\(x, y) -> ((a, y - x + a), x - a)) ps

pushUp :: [(Int, Int)] -> Int -> [(Int, Int)]
pushUp ps 0 = ps
pushUp ps c = pushUp (a ++ [(succ x, succ y)] ++ b) (pred c)
  where
  p@(x, y) = minimumBy (compare `on` snd) ps
  (a, (_:b)) = span (/= p) ps
  
