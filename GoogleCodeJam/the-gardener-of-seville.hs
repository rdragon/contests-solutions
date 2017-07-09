-- 2016-06-17
{-# LANGUAGE PatternSynonyms #-}

import qualified Data.Set as Set
import Data.List
import Control.Monad
import qualified Data.Vector as V
type Vec = V.Vector Int
data A = FS | BS -- FS: forward slash, BS: backward slash
data B = In | Out -- In: courtier is entering the given cell from given direction, Out: courtier is leaving the given cell to given direction

pattern North :: [Int]
pattern North = [-1, 0]
pattern South :: [Int]
pattern South = [1, 0]
pattern East :: [Int]
pattern East = [0, 1]
pattern West :: [Int]
pattern West = [0, -1]

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    [r, c] <- fmap (map read . words) getLine :: IO [Int]
    love <- fmap (V.fromList . (0 :) . map snd . sort . f . map read . words) getLine :: IO Vec
    putStrLn $ "Case #" ++ show i ++ ":"
    putStrLn $ solve r c love where
  f (i:j:is) = (i, j) : (j, i) : f is
  f _ = []
solve :: Int -> Int -> Vec -> String
solve r c love
  | not (check 1 n) = "IMPOSSIBLE"
  | otherwise = g $ f Set.empty Set.empty Set.empty
  where
    n = length love - 1
    startv = V.fromList . (undefined :) $ [([1, j], North) | j <- [1..c]] ++ [([i, c], East) | i <- [1..r]] ++ [([r, j], South) | j <- [c,c-1..1]] ++ [([i, 1], West) | i <- [r,r-1..1]]
    check i k
      | k < i = True
      | j > i && j <= k = check (succ i) (pred j) && check (succ j) k
      | otherwise = False where
        j = love V.! i
    g Nothing = "IMPOSSIBLE"
    g (Just for) = intercalate "\n" [[if [i, j] `Set.member` for then '/' else '\\' | j <- [1..c]] | i <- [1..r]]
    f for bac happy -- for: positions of forward slashes, bac: backward slashes, happy: courtiers that have found each other
      | length happy == n = Just for
      | otherwise = f1 for bac happy 1
    f1 for bac happy i
      | i == n + 1 = undefined
      | i `Set.notMember` happy && all (`Set.member` happy) ks = walk for bac happy (startv V.! i) In (startv V.! j) i j
      | otherwise = f1 for bac happy (succ i)
      where
        j = love V.! i
        ks = if j > i then [succ i .. pred j] else [succ i .. n] ++ [1 .. pred j]
    walk for bac happy a Out b i j
      | a == b = f for bac (i `Set.insert` (j `Set.insert` happy))
      | otherwise = walk for bac happy (step a) In b i j
    walk for bac happy a@(p@[i, j], d) In b i' j'
      | i < 1 || i > r || j < 1 || j > c = Nothing
      | p `Set.member` for = walk for bac happy (rotate FS a) Out b i' j'
      | p `Set.member` bac = walk for bac happy (rotate BS a) Out b i' j'
      | d == East || d == West = walk (p `Set.insert` for) bac happy a In b i' j' -- we alway create hedges for an anti-clockwise walk
      | otherwise = walk for (p `Set.insert` bac) happy a In b i' j'
    walk _ _ _ _ _ _ _ _ = undefined
    -- step: Out -> In
    step (p, d) = (plus p d, flipDir d)
    plus = zipWith (+)
    flipDir = map (* (-1))
    -- rotate: In -> Out
    rotate FS (p, North) = (p, West)
    rotate BS (p, North) = (p, East)
    rotate FS (p, South) = (p, East)
    rotate BS (p, South) = (p, West)
    rotate FS (p, East) = (p, South)
    rotate BS (p, East) = (p, North)
    rotate FS (p, West) = (p, North)
    rotate BS (p, West) = (p, South)
    rotate _ _ = undefined
    
