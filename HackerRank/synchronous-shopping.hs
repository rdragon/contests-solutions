-- 2016-05-25
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Bits
import Data.List
import Data.Maybe
import Data.Function

type Center = Int
type Fish = Int
type Road = (Center, Time)
type Time = Int
type Vertex = (Center, Fish)
type Arrival = (Time, Vertex)

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
run fish roads goal =
  let
    ars = Set.singleton (0, (0, 0)) :: Set.Set Arrival -- arrivals
    lu = Map.empty :: Map.Map Vertex Arrival -- lookup
    bought = [] :: [Fish] -- each time we hit center n we add one fish bitmask to this list
    vis = Set.empty :: Set.Set Vertex -- visited vertices
  in walk ars lu bought vis
  where
    n = pred $ V.length fish
    walk ars lu bought vis
      | c == n && foundAll = print t
      | otherwise = walk ars3 lu1 bought1 vis1
      where
        ((t, v@(c, f)), ars1) = Set.deleteFindMin ars
        foundAll = f == goal || any (\f1 -> f .|. f1 == goal) bought
        bought1 = if c == n then (f : bought) else bought
        ars2 = foldl' (flip Set.delete) ars1 arsDelete
        ars3 = foldl' (flip Set.insert) ars2 arsInsert
        lu1 = foldl' (\lu' a1@(_, v1) -> Map.insert v1 a1 lu') lu arsInsert
        ars4 = filter ((`Set.notMember` vis) . snd) [(t + t1, (c1, f .|. (fish V.! c1))) | (c1, t1) <- roads V.! c]
        pairs = zip ars4 [Map.lookup v1 lu | (_, v1) <- ars4]
        (new1, rest) = partition (isNothing . snd) pairs
        pairsDup = [(x, y) | (x, Just y) <- rest]
        (new2, arsDelete) = unzip . filter (uncurry ((<) `on` fst)) $ pairsDup
        arsInsert = map fst new1 ++ new2
        vis1 = Set.insert v vis
        
        
        