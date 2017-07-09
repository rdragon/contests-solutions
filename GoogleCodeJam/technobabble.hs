-- 2016-06-20
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

data Bigraph a = Bigraph (Set.Set a) (Map.Map a [a])

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    n <- fmap read getLine :: IO Int
    pairs <- fmap (map ((\[x, y] -> ['1' : x, '2' : y]) . words)) . sequence . replicate n $ getLine :: IO [[String]]
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve pairs)

solve :: [[String]] -> Int
solve pairs = length pairs - (n - 2 * k + k) where
  k = length . fordFulkerson $ Bigraph u e
  u = Set.fromList . map head $ pairs
  n = length u + (length . nub . map last $ pairs)
  e = foldr f Map.empty pairs
  f [x, y] = g x y . g y x
  f _ = undefined
  g x y = Map.insertWith (++) x [y]

fordFulkerson :: forall a. (Ord a, Show a) => Bigraph a -> Set.Set (a, a)
fordFulkerson (Bigraph u nbs) = grow u Set.empty where
  grow :: Set.Set a -> Set.Set (a, a) -> Set.Set (a, a)
  grow u' m = case foldl' g (Left Set.empty) . Set.toList $ u' of
    Right p -> grow (Set.delete (head p) u') . symDif m . Set.fromList . makeE $ p
    _ -> m
    where
      g (Left e) x = getP e [x]
      g x _ = x
      getP :: Set.Set (a, a) -> [a] -> Either (Set.Set (a, a)) [a]
      getP e0 p@(x:_) = foldl' f (Left e0) . (filter (\y -> (x, y) `Set.notMember` m)) . getNbs $ x
        where
        f (Left e) y = case listToMaybe . filter (\z -> (z, y) `Set.member` m) . getNbs $ y of
          Nothing -> Right . reverse $ (y:p)
          Just z -> if (z, y) `Set.member` e then Left e else getP ((z, y) `Set.insert` e) (z:y:p)
        f z _ = z
      getP _ _ = undefined
  makeE :: [a] -> [(a, a)]
  makeE (x0:y0:zs0) = (x0, y0) : f (y0:zs0) where
    f (y:x:y':zs) = (x, y) : (x, y') : f (y':zs)
    f [_] = []
    f _ = undefined
  makeE _ = undefined
  getNbs x = fromMaybe [] (Map.lookup x nbs)
  symDif x y = (x Set.\\ y) `Set.union` (y Set.\\ x)
