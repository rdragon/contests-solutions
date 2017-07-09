-- 2016-09-06
-- fairland, code jam 2015
{-# LANGUAGE BangPatterns #-}
import qualified Data.Set as Set
import Data.List
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Control.Monad

data Vertex = Vertex {ind :: Int, sal :: Int, par :: Int, chi :: [Int]} deriving (Show)

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rd :: IO [Int]
rd = fmap (map read . words) getLine

solveM :: IO String
solveM = do
  [n, d] <- rd
  [s0, sa, sc, sr] <- rd
  [m0, ma, mc, mr] <- rd
  let
    sals = take n $ iterate (\x -> (x * sa + sc) `mod` sr) s0
    parents = take n $ zipWith (\i x -> x `mod` i) (1 : [1..]) $ iterate (\x -> (x * ma + mc) `mod` mr) m0
    children = V.toList $ V.accum (++) (V.replicate n []) $ zip (tail parents) $ map return [1 :: Int ..]
    vs = V.fromList $ zipWith4 (\i x y z -> Vertex i x y z) [0..] sals parents children
  return $ show $ compute d vs (s0 - d) s0 Set.empty Map.empty Map.empty

compute :: Int -> V.Vector Vertex -> Int -> Int -> Set.Set Int -> Map.Map Int [Int] -> Map.Map Int [Int] -> Int
compute d vs sMin sMarie = run
  where
  -- st: set of all vertices in current subtree
  -- a: contains all vertices in st, indexed by salary (plus possible vertices already deleted from st)
  -- b: contains all vertices that are children of vertices in st but not themselves in st, and with a salary strictly above any salary in st, indexed by salary
  run !st !a !b
    | sMin > sMarie = 0
    | otherwise = max (Set.size st1) $ compute d vs (succ sMin) sMarie st1 a1 b1
    where
    (st0, a0) = deleteBelow st a
    (st1, a1, b1)
      | sMin == sMarie - d = addVertex (st, a, b) (V.head vs)
      | otherwise = addFromB st0 a0 b
  deleteBelow st a
    | Map.null a || s >= sMin = (st, a)
    | otherwise = (foldl' deleteVertex st $ map (vs V.!) is, a')
    where
    ((s, is), a') = Map.deleteFindMin a
  deleteVertex !st !v
    | (ind v) `Set.notMember` st = st
    | otherwise = foldl' deleteVertex st' $ map (vs V.!) $ chi v
    where
    st' = Set.delete (ind v) st
  addFromB st a b
    | Map.null b || s > sMin + d = (st, a, b)
    | otherwise = foldl' addVertex (st, a, b') $ filter (\v -> par v `Set.member` st) $ map (vs V.!) is
    where
    ((s, is), b') = Map.deleteFindMin b
  addVertex (!st, !a, !b) v = foldl' addVertex r ws0
    where
    r =
      ( Set.insert (ind v) st
      , Map.insertWith (++) (sal v) [(ind v)] a
      , foldl' (\mp w -> Map.insertWith (++) (sal w) [(ind w)] mp) b ws1 )
    (ws0, ws1) = partition (\w -> sal w <= sMin + d) $ filter (\w -> sal w >= sMin) $ map (vs V.!) $ chi v
    
