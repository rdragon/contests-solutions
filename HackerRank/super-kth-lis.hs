-- 2016-05-19
{-# LANGUAGE ScopedTypeVariables #-}

-- via editorial
module Main where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.List
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Bits
import Data.Function
import Data.Maybe

main :: IO ()
main = do
  [_, k] <- fmap (map read . words) getLine :: IO [Int]
  as <- fmap (U.fromList . map read . words) getLine :: IO A
  let gs = stage1 as
  let rcs = stage2 as gs
  putStrLn (stage3 as gs rcs k)
  
type A = UVec Int
type G = [[Int]]
type C = UVec Int
type UVec = U.Vector
type UMVec s a = UM.MVector s a

stage1 :: A -> G
stage1 as = runST $ do
  bs <- UM.replicate (succ n) 0
  gs <- M.replicate (succ n) []
  let
    f [] = return []
    f ((i, a) : xs) = do
      l <- fmap (succ . maximum . (0:)) $ query bs (pred a)
      update bs (max l) a
      M.modify gs (i:) l
      fmap (l:) (f xs)
  ls <- f $ zip [0..] (U.toList as)
  fmap (take (maximum ls) . tail . V.toList) (V.freeze gs)
  where
    n = U.length as

stage2 :: A -> G -> C
stage2 as gs = runST $ do
  bs <- UM.replicate (succ n) 0
  rcs <- UM.new n -- right counts
  sequence_ [UM.write rcs i 1 | i <- last gs]
  let
    handleGroup (g1:gs1) (g2:gs2) = do
      writeCounts g1 g2
      sequence_ [clear bs (succ n - getA i) | i <- g2]
      handleGroup gs1 gs2
    handleGroup _ _ = return ()
    writeCounts (i:g1) g2 = do
      updateBs g3
      rc <- fmap (foldl' add 0) $ query bs (succ n - succ (getA i))
      UM.write rcs i rc
      writeCounts g1 g4
      where
        (g3, g4) = span (> i) g2
    writeCounts _ _ = return ()
    updateBs [] = return ()
    updateBs (i:g1) = do
      rc <- UM.read rcs i
      update bs (add rc) (succ n - getA i)
      updateBs g1
  handleGroup (tail . reverse $ gs) (reverse gs)
  U.freeze rcs
  where
    getA = (as U.!)
    n = U.length as

stage3 :: A -> G -> C -> Int -> String
stage3 as gs rcs k = runST $ do
  bs <- UM.replicate (n + 2) 0
  let
    go _ [] _ = return []
    go k1 (dg:dgs1) ii = do
      mb <- findIi a_mb dg k1
      case mb of
        Nothing -> return []
        (Just (jj_lcs, k2)) -> do
          sequence_ [clear bs (succ i) | i <- ii]
          sequence_ [update bs (add lc) (succ j) | (j, lc) <- jj_lcs]
          let b = getA . fst . head $ jj_lcs
          fmap (b :) (go k2 dgs1 (map fst jj_lcs))
      where a_mb = if null ii then Nothing else (Just (getA (head ii)))
    findIi _ [] _ = return Nothing
    findIi a_mb (ii:iis) k1
      | isJust a_mb && b <= a = findIi a_mb iis k1
      | otherwise = do
        lcs <- if isJust a_mb then sequence [fmap (foldl' add 0) (query bs (succ i)) | i <- ii] else return (repeat 1) -- left counts
        let k2 = foldl' add 0 [mult lc (getRCount i) | (lc, i) <- zip lcs ii]
        if k2 < k1 then findIi a_mb iis (k1 - k2) else return (Just (zip ii lcs, k1))
      where
        b = getA (head ii)
        (Just a) = a_mb
  xs <- go k dgs []
  return $ if null xs then "-1" else (intercalate " " (map show xs))
  where
    n = U.length as
    -- divided groups (groups = indices grouped by left-length, divided = every group is again grouped, now by a-value. these subgroups we call inverse images)
    dgs = [groupBy ((==) `on` getA) . sortBy (compare `on` getA) $ g1 | g1 <- gs]
    getA = (as U.!)
    getRCount = (rcs U.!)
    -- lc = left count = number of ways to reach this position      

query :: PrimMonad m => UMVec (PrimState m) Int -> Int -> m [Int]
query bs i = sequence [UM.read bs j | j <- takeWhile (> 0) (iterate (\x -> x - (x .&. (-x))) i)]

update :: PrimMonad m => UMVec (PrimState m) Int -> (Int -> Int) -> Int -> m ()
update bs f i = sequence_ [UM.modify bs f j | j <- takeWhile (< UM.length bs) (iterate (\x -> x + (x .&. (-x))) i)]

clear :: PrimMonad m => UMVec (PrimState m) Int -> Int -> m ()
clear bs i = sequence_ [UM.write bs j 0 | j <- takeWhile (< UM.length bs) (iterate (\x -> x + (x .&. (-x))) i)]

kBound :: Int
kBound = 10 ^ (18 :: Int) + 1

add :: Int -> Int -> Int
add x y = min (x + y) kBound

mult :: Int -> Int -> Int
mult _ 0 = 0
mult x y = if x > 1 + (kBound `quot` y) then kBound else (x * y)
