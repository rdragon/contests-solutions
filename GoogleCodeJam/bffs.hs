-- 2016-06-16
import Data.Maybe
import Control.Monad
import Data.List
import qualified Data.Set as Set
import qualified Data.Sequence as S
import qualified Data.Map as Map
import Data.Foldable

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    _ <- getLine
    bff <- fmap (S.fromList . map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ show (solve bff)

solve :: S.Seq Int -> Int
solve bff = max sm0 $ getMx set0 [1..n] 0 where
  n = length bff
  pairs = zip [1..] . toList $ bff
  getBff = (bff `S.index`) . pred
  twins = filter (uncurry f) pairs where -- for each twin i, j, the longest directed path ending in i plus the one ending in j is part of the circle. exception: when there exists a cycle with a length greater than the sum of all these paths. 
    f i j = i < j && getBff j == i
  inv = foldr (uncurry f) Map.empty pairs where
    f i j = Map.insertWith (++) j [i]
  getInv = fromMaybe [] . (flip Map.lookup inv) -- getInverse i: all kids with bff i
  getSm :: Int -> Int -> (Int, Set.Set Int) -- getSum
  getSm i' j' = (m1 + m2, set1 `Set.union` set2) where
    (m1, set1) = f i' j'
    (m2, set2) = f j' i'
    f i j = g (Set.singleton i) (getInv i \\ [j]) 1
    g set [] m = (m, set)
    g set xs m = g (set `Set.union` (Set.fromList xs)) ys (succ m) where
      ys = concatMap getInv xs
  (sm0, set0) = foldr (uncurry f) (0, Set.empty) twins where
    f i j (sm, set) = let (x, y) = getSm i j in (sm + x, set `Set.union` y)
  getMx :: Set.Set Int -> [Int] -> Int -> Int -- getMax, get length of max cycle
  getMx _ [] m = m
  getMx set (i:is) m
    | i `Set.member` set = getMx set is m
    | otherwise = let (set', m') = getCycle set [i] in getMx set' is (max m m')
  getCycle set (i:is)
    | i `Set.member` set = (set, fromMaybe 0 . fmap succ $ (i `elemIndex` is))
    | otherwise = getCycle (Set.insert i set) (getBff i : i : is)
  getCycle _ _ = undefined
