-- 2016-09-03
-- bilingual, code jam 2015
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.Tuple

type E = V.Vector [(Int, Int)]
type F = Map.Map (Int, Int) Int

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

solveM :: IO String
solveM = do
  n <- fmap read getLine :: IO Int
  ss <- replicateM n getLine
  let
    edges = getEdges n ss
    flow = fordFulkerson edges Map.empty
  return $ show $ sum [x | ((0, _), x) <- Map.assocs flow]

getEdges :: Int -> [String] -> E
getEdges n ss = V.fromList $ Map.elems $ Map.fromListWith (++) $ edges1 ++ edges2
  where
  ws = Set.fromList $ concatMap words ss
  ss' = map (map $ \w -> n + 2 * Set.findIndex w ws) $ map words ss
  edges1 = concat [[(i, [(succ i, 1)]), (succ i, [(i, 0)])] | i <- take (length ws) [n, n + 2..]]
  edges2 = concat $ [[(i, [(j, big)]), (succ j, [(i, big)]), (j, [(i, 0)]), (i, [(succ j, 0)])] | (i, line) <- zip [0..] ss', j <- Set.toList $ Set.fromList $ line]

fordFulkerson :: E -> F -> F
fordFulkerson edges flow
  | isNothing path_mb = flow
  | otherwise = fordFulkerson edges flow'
  where
  path_mb = findPath edges flow
  path = fromJust path_mb
  flow' = foldl' (\mp e -> (Map.insertWith (+) e 1 $ Map.insertWith (+) (swap e) (-1) mp)) flow $ zip path (tail path)

findPath :: E -> F -> Maybe [Int]
findPath edges flow
  | isNothing ix_mb = Nothing
  | otherwise = Just $ reverse $ 1 : go (reverse $ take (fromJust ix_mb) layers) 1
  where
  ix_mb = findIndex (1 `elem`) layers
  go [] _ = []
  go (layer : rest) k = i : go rest i
    where
    i = head [j | j <- layer, (x, c) <- edges V.! j, x == k, c - (Map.findWithDefault 0 (j, k) flow) > 0]
  layers = takeWhile (not . null) $ map fst $ iterate f ([0], Set.singleton 0)
  f (layer, hits) = (layer', hits')
    where
    layer' = [j | i <- layer, (j, c) <- edges V.! i, j `Set.notMember` hits, c - (Map.findWithDefault 0 (i, j) flow) > 0]
    hits' = foldl' (\set i -> Set.insert i set) hits layer'

big :: Int
big = 9999
