-- Assumption: For each side of a tile there is at most one other tile that matches that side.
-- Assumption: For each pair of tiles there is at most one way to combine those tiles.

import Data.List
import Data.Maybe
import Data.Ix
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Tile = [String]
type Pos = (Int, Int)
type Map = M.Map Pos Tile
type Set = S.Set Pos

main = do
    ((t:_):tss) <- (map (\ts -> concatMap (take 4 . iterate (reverse . transpose)) [ts, reverse ts]) . getTiles . lines) <$> getContents
    let bs = f . getSet . fst $ until (null . snd) addTile (M.singleton (0, 0) t, tss)
    print . minimum $ map (\b -> S.size . foldl' removeMonster b $ S.toList b) bs
    where f b = concatMap (take 4 . iterate (S.map (\(x, y) -> (y, -x)))) [b, S.map (\(x, y) -> (x, -y)) b]

getTiles :: [String] -> [Tile]
getTiles [] = []
getTiles us = let ((_:t), vs) = span (not . null) us in (t : getTiles (drop 1 vs))

addTile :: (Map, [[Tile]]) -> (Map, [[Tile]])
addTile (m, tss) = head $ (catMaybes $ map f ds) ++ [addTile (m', tss)]
    where ds = filter ((`M.notMember` m) . fst) . map (\((x, y), t) -> ((x, y - 1), t)) $ M.toList m
          m' = M.fromList . map (\((x, y), t) -> ((-y, x), reverse $ transpose t)) $ M.toList m
          f (p, t) = snd <$> find ((== last t) . head . fst) (concatMap (\ts -> map (\s -> (s, (M.insert p s m, delete ts tss))) ts) tss)

getSet :: Map -> Set
getSet m = S.fromList . concatMap f . M.toList $ (map (init . tail) . init . tail) <$> m
    where n = (length . head $ M.elems m) - 2
          ps = map (\(x, y) -> (y, -x)) $ range ((1, 1), (n, n))
          f ((x, y), t) = map ((\(z, w) -> (x * n + z, y * n + w)) . fst) . filter ((== '#') . snd). zip ps $ concat t
        
removeMonster :: Set -> Pos -> Set
removeMonster b (x, y) = if and $ map (`S.member` b) ps then S.filter (`notElem` ps) b else b
    where ps = map f [(0,0),(-1,1),(3,0),(4,1),(5,1),(6,0),(9,0),(10,1),(11,1),(12,0),(15,0),(16,1),(17,2),(17,1),(18,1)]
          f (z, w) = (x + z, y + w)