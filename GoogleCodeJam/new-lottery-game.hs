-- 2016-09-10
-- new lottery game, code jam 2014
import Data.List
import Text.Printf
import qualified Data.Map.Strict as Map
import Data.Bits
import Data.Char
import Control.Monad

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rdLine :: IO [Int]
rdLine = fmap (map read . words) getLine

solveM :: IO String
solveM = do
  xs <- fmap (map pred) rdLine
  return $ show $ solve $ map (map digitToInt . (printf "%030b")) xs

solve :: [[Int]] -> Int
solve xs = sum $ Map.elems $ foldl' compute (Map.singleton [EQ, EQ, EQ] 1) (transpose xs)

compute :: Map.Map [Ordering] Int -> [Int] -> Map.Map [Ordering] Int
compute mp abk = Map.fromListWith (+) 
  [ (pqr1, Map.findWithDefault 0 pqr mp)
  | pqr <- replicateM 3 [LT, EQ]
  , abk1 <- fmap (\[x, y] -> [x, y, x .&. y]) $ replicateM 2 [0, 1]
  , let pqr1 = zipWith3 cmp pqr abk1 abk
  , maximum pqr1 /= GT
  ]
  where
  cmp p a1 a = compare (p, a1) (EQ, a)
