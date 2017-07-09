-- 2016-11-07
-- the-bored-traveling-salesman, code jam 2014

import Data.List
import Data.Function
import Control.Monad
import qualified Data.Vector as V
import qualified Data.IntSet as Set

type Node = Int
type AdjVec = V.Vector NodeSet
type Weight = Int
type WeightVec = V.Vector Weight
type NodeSet = Set.IntSet
type Graph = (NodeSet, AdjVec, WeightVec)
type Path = [Node]

main :: IO ()
main = getLine >>= \s -> forM_ [(1 :: Int) .. read s] $ \i -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rdLine :: IO [Int]
rdLine = fmap (map read. words) getLine

solveM :: IO String
solveM = do
  [n, m] <- rdLine
  wv <- V.fromList. map head <$> replicateM n rdLine
  adj <- V.accum (flip Set.insert) (V.replicate n Set.empty). concatMap (\[i, j] -> [(i - 1, j - 1), (j - 1, i - 1)]) <$> replicateM m rdLine
  let 
    is = [0 .. n - 1]
    i = minimumBy (compare `on` (wv V.!)) is
    path = i : makeRoute (Set.fromList is, adj, wv) [i]
  return $ concatMap (show. (wv V.!)) path

makeRoute :: Graph -> Path -> Path
makeRoute (i_set, adj, wv) path
  | Set.size i_set == length path = []
  | otherwise = i_next : makeRoute (i_set', adj, wv) (i_next : path2)
  where
  i_potentials = sortBy (compare `on` (wv V.!)) (Set.toList i_set \\ path)
  first_in_paths = [findIndex ((i `Set.member`). (adj V.!)) path | i <- i_potentials]
  Just ix_i_next = findIndex f first_in_paths
    where
    f Nothing = False
    f (Just ix) = and $ take ix bools
  Just ix_in_path = first_in_paths !! ix_i_next
  i_next = i_potentials !! ix_i_next
  bools = computeBools (i_set, adj, wv) path
  (path1, path2) = splitAt ix_in_path path
  i_set' = foldr Set.delete i_set path1

computeBools :: Graph -> Path -> [Bool]
computeBools _ [] = []
computeBools (i_set, adj, wv) (i:is) = connected (i_set', adj, wv) : computeBools (i_set', adj, wv) is
  where
  i_set' = Set.delete i i_set

connected :: Graph -> Bool
connected (i_set, adj, _)
  | Set.null i_set = True
  | otherwise = go [head $ Set.elems i_set] Set.empty
  where
  go [] visited = Set.size visited == Set.size i_set
  go (i:is) visited
    | i `Set.member` visited = go is visited
    | otherwise = go (is ++ js) (Set.insert i visited)
    where
    js = filter (`Set.member` i_set) $ Set.toList (adj V.! i)
