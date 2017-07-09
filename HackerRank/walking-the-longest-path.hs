-- 2016-11-13
-- walking-the-longest-path, university codesprint
{-# LANGUAGE BangPatterns #-}
import System.CPUTime
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as M
import System.Random
import qualified Data.Set as Set

main :: IO ()
main = do
  setStdGen (mkStdGen 1)
  rd <- intReader
  [n, m] <- rd 2
  adj <- V.accum (flip (:)) (V.replicate n []) . concatMap (\[i, j] -> [(i - 1, j - 1), (j - 1, i - 1)]) <$> replicateM m (rd 2)
  --let { n = 500; m = 2500; }; adj <- V.accum (flip (:)) (V.replicate n []) . concatMap (\(i, j) -> [(i, j), (j, i)]) . Set.elems <$> mk n m 
  v <- M.replicate n False
  (k, xs) <- findBest n adj v (0, [])
  print k
  putStrLn . unwords . map (show . succ) $ xs

set :: M.IOVector Bool -> Int -> IO ()
set v i = M.write v i True

unset :: M.IOVector Bool -> Int -> IO ()
unset v i = M.write v i False

isSet :: M.IOVector Bool -> Int -> IO Bool
isSet v i = M.read v i

findBest :: Int -> V.Vector [Int] -> M.IOVector Bool -> (Int, [Int]) -> IO (Int, [Int])
findBest n adj v !best0 = do
  best1 <- go best0 (5 :: Int)
  x <- tle
  if x || fst best1 == n then return best1 else findBest n adj v best1
  where
  go !best 0 = return best
  go !best cnt = do
    start <- getStdRandom (randomR (0, (n - 1)))
    set v start
    (k1, xs) <- walk_from start (0, [])
    (k2, ys) <- walk_from start (0, [])
    unset v start
    sequence_ [unset v i | i <- xs ++ ys]
    let
      best' = if k1 + k2 + 1 > fst best then (k1 + k2 + 1, xs ++ (start : reverse ys)) else best
    go best' (cnt - 1)
  walk_from i (k, xs) = do
    js <- filterM (fmap not . isSet v) (adj V.! i)
    if null js then return (k, xs) else do
      ix <- getStdRandom (randomR (0, length js - 1))
      let j = js !! ix
      set v j
      walk_from j (k + 1, (j:xs))

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words). B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n. map (fst. fromJust. B.readInt) $ xs)

tle :: IO Bool
tle = do
  t <- fmap fromIntegral getCPUTime :: IO Double
  return $ (t / 10^(12 :: Int)) > 4.7

--mk :: Int -> Int -> IO (Set.Set (Int, Int))
--mk n m
--  | 2 * m > n * (n - 1) = undefined
--  | m < n - 1 = undefined
--  | otherwise = go st0
--  where
--  st0 = Set.fromList [(i, i + 1) | i <- [0..n-2]]
--  go st
--    | Set.size st == m = return st
--    | otherwise = do
--      i <- getStdRandom (randomR (0, n - 1))
--      j' <- getStdRandom (randomR (0, n - 2))
--      let 
--        j = if j' == i then n - 1 else j'
--        e = (min i j, max i j)
--        st1 = Set.insert e st
--      go st1