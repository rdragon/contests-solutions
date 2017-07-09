-- 2016-09-12
-- runaway quail, code jam 2015
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Tuple
import Data.List
import Control.Monad

data Quail = Quail { getIx :: Int, getP :: Double, getS :: Double }
type A = (Double, Double, Catch)
type Catch = (Int, Int)

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rdLine :: IO [Int]
rdLine= fmap (map read . words) getLine

solveM :: IO String
solveM = do
  [y, _] <- rdLine
  ps <- fmap (map fromIntegral) rdLine
  ss <- fmap (map fromIntegral) rdLine
  let
    (a0, b0) = partition ((< 0) . snd) $ zip ss ps
    a1 = map (fmap abs) a0
    b1 = map (fmap abs) b0
    a2 = V.fromList $ zipWith (\i (s, p) -> Quail i p s) [0..] $ reverse $ sort (a1 ++ [(0, 1e99)])
    b2 = V.fromList $ zipWith (\i (s, p) -> Quail i p s) [0..] $ reverse $ sort (b1 ++ [(0, 1e99)]) 
  return $ show $ dijkstra (fromIntegral y) a2 b2 (Set.singleton (0, 0, (0, 0))) Map.empty

dijkstra :: Double -> V.Vector Quail -> V.Vector Quail -> Set.Set A -> Map.Map Catch (Double, Double) -> Double
dijkstra y a b st0 mp0
  | c0 == (pred n, pred m) = r0
  | otherwise = dijkstra y a b st2 mp2
  where
  (st2, mp2) = foldl' f (st1, mp0) $ ma ++ mb
  f (st, mp) (t, r, c) = case Map.lookup c mp of 
    Nothing -> (Set.insert (t, r, c) st, Map.insert c (t, r) mp)
    Just (t', r') -> if t > t' then (st, mp) else (Set.insert (t, r, c) $ Set.delete (t', r', c) st, Map.insert c (t, r) mp)
  n = V.length a
  m = V.length b
  ((t0, r0, c0), st1) = Set.deleteFindMin st0
  ma = map (\(t, r, i) -> (t, r, (i, snd c0))) $ moves a y (fst c0) t0
  mb = map (\(t, r, i) -> (t, r, (fst c0, i))) $ moves b y (snd c0) t0

eps :: Double
eps = 0.000000001

moves :: V.Vector Quail -> Double -> Int -> Double -> [(Double, Double, Int)]
moves a y = go
  where
  go i t0 = case hits' of
    [] -> []
    ((_, q2) : _) -> (dt1 * 2 + t0, t0 + dt1, getIx q2) : go (getIx q2) t0
    where
    hits = map (\q -> ((abs (getP q) + (getS q) * t0) / (y - (getS q)), q)) $ V.toList $ V.drop i a
    (dt1, q1) = head hits
    hits' = filter (\(dt, q) -> getS q < getS q1 && dt > dt1 + eps) $ tail hits
