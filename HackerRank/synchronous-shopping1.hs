-- 2016-05-25
-- using mutable state for 'lu' and 'vis'. only 20% speedup. guess the modification of 'ars' takes most of the time

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Set as Set
import Data.Bits
import Data.List
import Data.Maybe
import Data.Function
import Control.Monad

type Center = Int
type Fish = Int
type Road = (Center, Time)
type Time = Int
type Vertex = (Center, Fish)
type Arrival = (Time, Vertex)
--main2 :: IO ()
--main2 = do
--  putStrLn "999 980 10"
--  sequence_ [putStrLn s | s <- replicate 98 "0" ++ ["1 1"] ++ concat [replicate 99 "0" ++ ["1 " ++ show i] | i <- [2..10]]]
--  sequence_ [doRow i | i <- [2, 102..902]]
--  sequence_ [putStrLn $ "1 " ++ show i ++ " 1" | i <- [2, 102..902]]
--  where
--    doRow i = sequence_ [putStrLn $ show j ++ " " ++ show (succ j) ++ " " ++ show k | (j, k) <- zip [i..i + 96] [2..]]
main :: IO ()
main = do
  [n, m, k] <- fmap (map read . words) getLine :: IO [Int]
  fish <- readFish n :: IO (V.Vector Fish)
  roads <- readRoads n m :: IO (V.Vector [Road])
  run fish roads (2 ^ k - 1)
  where
    readFish n = fmap (V.fromList . (0:) . map (foldl' (.|.) 0 . map ((2 ^) . pred . (read :: String -> Int)) . tail . words)) . sequence . replicate n $ getLine
    readRoads n m = do
      v <- M.replicate (succ n) []
      M.write v 0 [(1, 0)]
      sequence_ . replicate m $ addRoad v
      V.freeze v
    addRoad v = do
      [c1, c2, t] <- fmap (map read . words) getLine
      M.modify v ((c1, t) :) c2
      M.modify v ((c2, t) :) c1
run :: V.Vector Fish -> V.Vector [Road] -> Int -> IO ()
run fish roads goal = do
  let ars = Set.singleton (0, (0, 0)) :: Set.Set Arrival
  lu <- M.replicate (n * succ goal + 1) Nothing
  let  bought = [] :: [Fish]
  vis <- M.replicate (n * succ goal + 1) False
  walk ars lu bought vis
  where
    n = pred $ V.length fish
    getIndex (c, f) = f * n + c
    walk ars lu bought vis
      | c == n && foundAll = print t
      | otherwise = do
        ars4 <- filterM (fmap not . M.read vis . getIndex . snd) [(t + t1, (c1, f .|. (fish V.! c1))) | (c1, t1) <- roads V.! c]
        temp <- mapM (M.read lu . getIndex . snd) ars4
        let pairs = zip ars4 temp
        let (new1, rest) = partition (isNothing . snd) pairs
        let pairsDup = [(x, y) | (x, Just y) <- rest]
        let (new2, arsDelete) = unzip . filter (uncurry ((<) `on` fst)) $ pairsDup
        let arsInsert = map fst new1 ++ new2
        sequence_ [M.write lu (getIndex v1) (Just a1) | a1@(_, v1) <- arsInsert]
        M.write vis (getIndex v) True
        let ars2 = foldl' (flip Set.delete) ars1 arsDelete
        let ars3 = foldl' (flip Set.insert) ars2 arsInsert
        walk ars3 lu bought1 vis
      where
        ((t, v@(c, f)), ars1) = Set.deleteFindMin ars
        foundAll = f == goal || any (\f1 -> f .|. f1 == goal) bought
        bought1 = if c == n then (f : bought) else bought
        
        
        