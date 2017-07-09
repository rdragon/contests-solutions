-- 2016-04-17
{-# LANGUAGE BangPatterns #-}

import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Vector.Unboxed as Vec
import Data.List
import Data.Functor

main = do
  (p, q) <- getPQ
  let r = findR p
      !rLog = getRLog p r
      handleLine = do
        (k, n) <- getPQ
        let roots = if n `mod` p == 0 then [0] else getRoots p r (k `mod` (p - 1)) (rLog Vec.! (n `mod` p))
        putStrLn (if null roots then "NONE" else intercalate " " (map show roots))
  sequence_ $ replicate q handleLine
  where getPQ = do
          (s1,  ' ' : s2) <- span (/= ' ') <$> getLine
          return (read s1, read s2)
          
getRLog :: Int -> Int -> (Vec.Vector Int)
getRLog p r = runST $ do
  mv <- MVec.new p
  let r' = fromIntegral r :: Integer
      p' = fromIntegral p :: Integer
  sequence_ [MVec.write mv (fromInteger r'') a | (a, r'') <- zip [1 .. p - 1] (iterate (\x -> (x * r') `mod` p') r')]
  v <- fmap (\xs -> Vec.fromList (0 : xs)) $ sequence [MVec.read mv i | i <- [1 .. p - 1]]
  return v

inv :: Int -> Int -> Int
inv z x = (fromInteger y)
  where
    (_, y) = f (fromIntegral z) (fromIntegral x)
    f :: Integer -> Integer -> (Integer, Integer)
    f _ 1 = (0, 1)
    f a b =
      let (n, m') = a `quotRem` b
          (xb, xm) = f b m'
      in (xm, (xb - n * xm) `mod` (fromIntegral z))
  
getRoots p r k a
  = sort roots
  where
    m = gcd k (p - 1)
    w = (p - 1) `quot` m
    k1 = k `quot` m
    roots = if a `mod` m == 0 then roots' else []
    a1 = a `quot` m
    x1 = if k1 == 0 then 0 else (k1i * a1) `mod` w
    k1i = inv w k1
    exps = [x1 + w * i | i <- [0 .. m - 1]]
    roots' = [pow p r i | i <- exps]

findR 2 = 1
findR p =
  let qs = factors (p - 1)
  in head [r | r <- [2..p-1], all (/= 1) [pow p r ((p-1) `quot` q) | q <- qs]]

pow :: Int -> Int -> Int -> Int
pow p' r a = fromInteger $ f 1 (fromIntegral r) a
  where p = fromIntegral p'
        f :: Integer -> Integer -> Int -> Integer
        f x _ 0 = x
        f x r a = f x' ((r * r) `mod` p) a'
          where x' = if bit == 1 then ((x * r) `mod` p) else x
                (a', bit) = a `quotRem` 2

primes = 2 : 3 : [n | n <- [5,7..],
  all (\x -> n `mod` x /= 0) . takeWhile (\x -> x^2 <= n) $ primes]

factors n = f primes []
  where f (p : ps) qs
          | p * p > n = qs
          | otherwise = if n `mod` p == 0 then f ps (p : qs) else f ps qs
