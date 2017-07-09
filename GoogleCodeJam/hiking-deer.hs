-- 2016-08-22
import qualified Data.Map.Strict as Map
import Data.List
import Control.Monad
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [n] <- fmap (map read . words) getLine :: IO [Int]
    hikers <- fmap concat $ replicateM n readHikerGroup
    let ans = solve hikers
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans
  where
  readHikerGroup = do
    [d, h, m] <- fmap (map read . words) getLine :: IO [Int]
    return [(d, m') | m' <- take h [m..]]

solve hikers = minimum $ map (uncurry compute) $ addLc 0 0 0 $ reverse $ sort $ map (uncurry ft) hikers
  where
  hc = length hikers -- hiker count
  tc = succ hc * 2 + 2 -- times count
  ft d m = (360 - d) * m -- finish time
  -- add later count (number of hikers who finish strictly later)
  addLc _ _ _ [] = []
  addLc lc nLastC nLast (n : rest)
    | nLast == n = (n, lc) : addLc lc (succ nLastC) n rest
    | otherwise = (n, lc + nLastC) : addLc (lc + nLastC) 1 n rest
  -- a vector containing all the times any hiker passes the starting point (multiplied by 2)
  times = V.fromList $ take tc $ go $ 
      Map.fromList [((ft d m, ix), m) | (ix, (d, m)) <- zip [0 :: Int ..] hikers]
    where
    go mp = (n * 2) : go mp''
      where
      (((n, ix), m), mp') = Map.deleteFindMin mp
      mp'' = Map.insert (n + m * 360, ix) m mp'
  -- finish count (total number of times the hikers pass the starting point upto time n)
  fc n = go 0 tc
    where
    p = n * 2 + 1
    go i j
      | i == j = i
      | q > p = go i k
      | otherwise = go (succ k) j
      where
      k = i + (j - i) `quot` 2
      q = times V.! k
  -- compute the number of encounters given a finish time and a later count
  compute n lc = lc + fc n - (hc - lc)
