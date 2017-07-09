-- 2016-05-01
import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.ST
import Data.List

type Rod = Int
type Disc = Int
type Configuration = Vec.Vector Rod

-- return the index of a configuration in the memoization vector 
index :: Configuration -> Int
index c = sum (zipWith (*) (Vec.toList c) (iterate (*4) 1))

-- return the top-most disc of a rod
top :: Configuration -> Rod -> Maybe Disc
top c i = i `Vec.elemIndex` c

-- we need to make use of the symmetry of the three rods exluding the first rod
-- this allows for a reduction of the number of positions with a factor ~6
-- is just what was needed to stay below the time limit
representative :: Configuration -> Configuration
representative c
  | x >= y && y >= z = c
  | x >= z && z >= y = f [0, 1, 3, 2]
  | y >= x && x >= z = f [0, 2, 1, 3]
  | y >= z && z >= x = f [0, 2, 3, 1]
  | z >= x && x >= y = f [0, 3, 1, 2]
  | z >= y && y >= x = f [0, 3, 2, 1]
  where
    x = bottom 1
    y = bottom 2
    z = bottom 3
    bottom = fromMaybe 10 . top c'
    c' = Vec.reverse c
    f perm = Vec.map (fromJust . (`elemIndex` perm)) c

-- move top-most disc from rod i to rod j
-- if this is not a valid move, return Nothing
move :: Configuration -> Rod -> Rod -> Maybe Configuration
move c i j
  | is_valid_move = (Just d')
  | otherwise = Nothing
  where
    is_valid_move
      | isJust x && (isNothing y || fromJust x < fromJust y) = True
      | otherwise = False
    x = top c i
    y = top c j
    z = i `Vec.elem` (Vec.drop (fromJust x + 1) c)
    d = c Vec.// [(fromJust x, j)]
    d' = if (i == 0 || z) && (j == 0 || isJust y) then d else (representative d)

main :: IO ()
main = do
  n <- fmap read getLine
  c <- fmap (Vec.fromList . fmap (pred . read) . words) getLine
  print (run n (representative c))
    
run :: Int -> Configuration -> Int
run n c = runST $ do
  s <- MVec.replicate (4^n) (-1 :: Int) -- the memoization vector
  MVec.write s 0 0 -- the initial configuration requires no moves
  let
    -- f cs dss m
    --   cs: all configurations yet to enumerate that were reachable in (m - 1) moves
    --   concat dss: all configurations found thus far that are reachable in m moves
    f [] [] _ = return ()
    f [] dss m = f (concat dss) [] (m + 1)
    f (c:cs) dss m = do
      let
        ds' = [fromJust d | i <- [0..3], j <- [0..3], i /= j, let d = move c i j, isJust d]
      ds_mb <- forM ds' $ \d -> do
        m' <- MVec.read s (index d)
        ans <- if m' /= -1 then return Nothing else do
          MVec.write s (index d) m
          return (Just d)
        return ans
      let
        ds = fmap fromJust . filter isJust $ ds_mb
      f cs (ds:dss) m
  f [Vec.replicate n 0] [] 1 -- the first argument is a list with the initial configuration
  m <- MVec.read s (index c)
  return m
  
  
-----------------------------------------------------------------------------------------------  
-- MORE

-- check some optimizations
-- Int8 for Rod such that Configuration is 8 times smaller: no difference
-- slower index version (two times as many multiplications): 25% slower
-- slower top function: 70% slower

import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.ST
import Data.List
import Data.Int

type Rod = Int8
type Disc = Int8
type Configuration = Vec.Vector Rod

-- return the index of a configuration in the memoization vector 
index :: Configuration -> Int
index c = sum (zipWith (*) (fmap fromIntegral (Vec.toList c)) (iterate (*4) 1))

-- -- slower verion
-- index :: Configuration -> Int
-- index c = sum (zipWith (*) digits (iterate (*2) 1))
--   where
--     digits = concatMap f (Vec.toList c)
--     f 0 = [0, 0]
--     f 1 = [0, 1]
--     f 2 = [1, 0]
--     f 3 = [1, 1] 

-- return the top-most disc of a rod
top :: Configuration -> Rod -> Maybe Disc
top c i = fmap fromIntegral (i `Vec.elemIndex` c)

-- -- slower version
-- top :: Configuration -> Rod -> Maybe Disc
-- top c i = if null b then Nothing else (Just z)
--   where
--     a = zip (Vec.toList c) [0..]
--     b = filter (\(x, y) -> x == i) a
--     z = minimum (map snd b)
    



-- -- we need to make use of the symmetry of the three rods exluding the first rod
-- -- this allows for a reduction of the number of positions with a factor ~6
-- -- is just what was needed to stay below the time limit
-- representative :: Configuration -> Configuration
-- representative c
--   | x >= y && y >= z = c
--   | x >= z && z >= y = f [0, 1, 3, 2]
--   | y >= x && x >= z = f [0, 2, 1, 3]
--   | y >= z && z >= x = f [0, 2, 3, 1]
--   | z >= x && x >= y = f [0, 3, 1, 2]
--   | z >= y && y >= x = f [0, 3, 2, 1]
--   where
--     x = bottom 1 :: Disc
--     y = bottom 2 :: Disc
--     z = bottom 3 :: Disc
--     bottom = fromMaybe 10 . top c'
--     c' = Vec.reverse c
--     f perm = Vec.map (fromIntegral . fromJust . (`elemIndex` perm)) c

-- move top-most disc from rod i to rod j
-- if this is not a valid move, return Nothing
move :: Configuration -> Rod -> Rod -> Maybe Configuration
move c i j
  | is_valid_move = (Just d')
  | otherwise = Nothing
  where
    is_valid_move
      | isJust x_mb && (isNothing y_mb || x < y) = True
      | otherwise = False
    x_mb = top c i
    x = fromJust x_mb
    y_mb = top c j
    y = fromJust y_mb
    x_int = fromIntegral x
    z = i `Vec.elem` (Vec.drop (x_int + 1) c)
    d' = move_disk c x j
    -- d' = if (i == 0 || z) && (j == 0 || isJust y_mb) then d else (representative d)
    
move_disk :: Configuration -> Disc -> Rod -> Configuration
move_disk c x i = c Vec.// [(fromIntegral x, i)]

main :: IO ()
main = do
  n <- fmap read getLine
  c <- fmap (Vec.fromList . fmap (pred . read) . words) getLine
  -- print (run n (representative c))
  print (run n c)
    
run :: Int -> Configuration -> Int
run n c = runST $ do
  s <- MVec.replicate (4^n) (-1 :: Int) -- the memoization vector
  MVec.write s 0 0 -- the initial configuration requires no moves
  let
    -- f cs dss m
    --   cs: all configurations yet to enumerate that were reachable in (m - 1) moves
    --   concat dss: all configurations found thus far that are reachable in m moves
    f [] [] _ = return ()
    f [] dss m = f (concat dss) [] (m + 1)
    f (c:cs) dss m = do
      let
        ds' = [fromJust d | i <- [0..3], j <- [0..3], i /= j, let d = move c i j, isJust d]
      ds_mb <- forM ds' $ \d -> do
        m' <- MVec.read s (index d)
        ans <- if m' /= -1 then return Nothing else do
          MVec.write s (index d) m
          return (Just d)
        return ans
      let
        ds = fmap fromJust . filter isJust $ ds_mb
      f cs (ds:dss) m
  f [Vec.replicate n 0] [] 1 -- the first argument is a list with the initial configuration
  m <- MVec.read s (index c)
  return m