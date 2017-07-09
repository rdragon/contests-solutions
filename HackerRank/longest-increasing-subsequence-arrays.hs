-- 2016-07-24
-- het aantal manieren is gelijk aan som_{k=n-1}^{m-1} (n-1)^{k-(n-1)} n^{m-k} (k `choose` n-1) en deze som berekenen we

import Data.List
import Control.Monad
import qualified Data.Vector as V

main :: IO ()
main = do
  pairsC <- fmap read getLine :: IO Int
  pairs <- replicateM pairsC $ fmap ((\[m, n] -> (m, n)) . map read . words) getLine :: IO [(Int, Int)]
  let
    maxM = maximum $ map fst $ pairs
    invs = V.fromList $ [inv i | i <- [0 .. maxM]]
    inv' = (invs V.!)
    go (m, n) = f 1 (modpow n (m - n)) 1 (pred n)
      where
      ni = inv' n
      f a b c k
        | k == m = 0
        | otherwise = (a `mult` b `mult` c) `add` f a1 b1 c1 (succ k)
        where
        a1 = a `mult` (pred n)
        b1 = b `mult` ni
        c1 = c `mult` (succ k) `mult` (inv' (k - n + 2))
  mapM_ print (map go pairs)

modulo :: Int
modulo = 1000000007

mult :: Int -> Int -> Int
mult x y = (x * y) `mod` modulo

add :: Int -> Int -> Int
add x y = (x + y) `mod` modulo

inv :: Int -> Int
inv x = modpow x (modulo - 2)

modpow :: Int -> Int -> Int
modpow a b = go 1 b a
  where
  go x 0 _ = x
  go x b1 a1 = go y q (mult a1 a1)
    where
    (q, r) = b1 `divMod` 2
    y = if r == 0 then x else mult x a1