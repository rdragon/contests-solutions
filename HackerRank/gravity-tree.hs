-- 2016-09-17
-- gravity tree, week of code 23
import System.Random
import Data.Function
import Data.List
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  parents <- fmap (U.fromList . map pred . (1 :)) $ rd $ pred n
  let
    childrens = V.accum (flip (:)) (V.replicate n []) $ zip (tail $ U.toList parents) [1..]
    weights = let v = V.generate n $ \i -> succ $ sum $ map (v V.!) $ children i in U.convert v
    dsums = let v = V.generate n $ \i -> (pred $ weight i) + (sum $ map (v V.!) $ children i) in U.convert v
    dbelows = let v = V.generate n $ \i -> if i == 0 then 0 else (let p = parent i in n - 2 * weight i + dsum p - dsum i + (v V.! p)) in U.convert v
    ddsums = let v = V.generate n $ \i -> (2 * dsum i) - (pred $ weight i) + (sum $ map (v V.!) $ children i) in U.convert v
    ddbelows = let v = V.generate n $ \i -> if i == 0 then 0 else (let p = parent i in 2 * dbelow i - n + (v V.! p) + ddsum p - ddsum i - 2 * dsum i) in U.convert v
    depths = let v = V.generate n $ \i -> if i == 0 then 0 else succ (v V.! parent i) in U.convert v
    jumps = U.accum (flip const) (U.replicate n 0) $ f 0 0 []
      where
      f i j xs = (i, j) : (foldl' (flip ($)) xs $ zipWith f is $ j : tail is)
        where
        is = reverse $ sortBy (compare `on` weight) $ children i
    children :: Int -> [Int]
    children = (childrens V.!)
    parent, weight, dsum, dbelow, ddsum, ddbelow, depth, jump :: Int -> Int
    parent = (parents U.!)
    weight = (weights U.!)
    dsum = (dsums U.!)
    dbelow = (dbelows U.!)
    ddsum = (ddsums U.!)
    ddbelow = (ddbelows U.!)
    depth = (depths U.!)
    jump = (jumps U.!)
    findRoot i j = case depth i' `compare` depth j' of
      LT -> findRoot i $ parent j'
      GT -> findRoot j $ parent i'
      EQ
        | i' /= j' -> findRoot (parent i') $ parent j'
        | depth i < depth j -> i
        | otherwise -> j
      where
      i' = jump i
      j' = jump j
  [q] <- rd 1
  (us, vs) <- fmap (U.unzip . U.fromList) $ replicateM q $ fmap ((\[u, v] -> (u, v)) . map pred) $ rd 2
  let
    ws_v = U.map weight vs
    ds_v = U.map dsum vs
    dds_v = U.map ddsum vs
    dds_u = U.map ddsum us
    ddbs_u = U.map ddbelow us
    ddbs_v = U.map ddbelow vs
    dbs_v = U.map dbelow vs
    rs = U.zipWith findRoot us vs
    ls = U.zipWith3 (\u v r -> depth u + depth v - 2 * depth r) us vs rs
    xs = U.zipWith4 (\l w d dd -> l * l * w + 2 * l * d + dd) ls ws_v ds_v dds_v
    ys = U.zipWith6 (\dd_v ddb_u ddb_v l w_v db_v -> dd_v + ddb_u - ddb_v - l * l * (n - w_v) - 2 * l * db_v) dds_u ddbs_u ddbs_v ls ws_v dbs_v
    zs = U.zipWith4 (\v r x y -> if v /= r then x else y) vs rs xs ys
  putStrLn $ unlines $ map show $ U.toList zs

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)

{-
dit is eigenlijk wat ik zou willen typen. hetzelfde algoritme, maar zonder geheugen optimalisaties

import System.Random
import Data.Function
import Data.List
import qualified Data.Vector as V
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  parents <- fmap (V.fromList . map pred . (1 :)) $ rd $ pred n
  let
    childrens = V.accum (flip (:)) (V.replicate n []) $ zip (tail $ V.toList parents) [1..]
    weights = V.generate n $ \i -> succ $ sum $ map weight $ children i
    dsums = V.generate n $ \i -> (pred $ weight i) + (sum $ map dsum $ children i)
    dbelows = V.generate n $ \i -> if i == 0 then 0 else (let p = parent i in n - 2 * weight i + dsum p - dsum i + (dbelow p))
    ddsums = V.generate n $ \i -> (2 * dsum i) - (pred $ weight i) + (sum $ map ddsum $ children i)
    ddbelows = V.generate n $ \i -> if i == 0 then 0 else (let p = parent i in 2 * dbelow i - n + ddbelow p + ddsum p - ddsum i - 2 * dsum i)
    depths = V.generate n $ \i -> if i == 0 then 0 else succ (depth $ parent i)
    jumps = V.accum (flip const) (V.replicate n 0) $ f 0 0 []
      where
      f i j xs = (i, j) : (foldl' (flip ($)) xs $ zipWith f is $ j : tail is)
        where
        is = reverse $ sortBy (compare `on` weight) $ children i
    children :: Int -> [Int]
    children = (childrens V.!)
    parent, weight, dsum, dbelow, ddsum, ddbelow, depth, jump :: Int -> Int
    parent = (parents V.!)
    weight = (weights V.!)
    dsum = (dsums V.!)
    dbelow = (dbelows V.!)
    ddsum = (ddsums V.!)
    ddbelow = (ddbelows V.!)
    depth = (depths V.!)
    jump = (jumps V.!)
    findRoot i j = case depth i' `compare` depth j' of
      LT -> findRoot i $ parent j'
      GT -> findRoot j $ parent i'
      EQ
        | i' /= j' -> findRoot (parent i') $ parent j'
        | depth i < depth j -> i
        | otherwise -> j
      where
      i' = jump i
      j' = jump j
    solve u v
      | r /= v = l * l * weight v + 2 * l * dsum v + ddsum v
      | otherwise = ddsum u + ddbelow u - ddbelow v - l * l * (n - weight v) - 2 * l * dbelow v
      where
      r = findRoot u v
      l = depth u + depth v - 2 * depth r
  [q] <- rd 1
  replicateM_ q $ do
    [u, v] <- fmap (map pred) $ rd 2
    print $ solve u v

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)

-}
