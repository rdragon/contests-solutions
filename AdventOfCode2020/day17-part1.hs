import qualified Data.Set as S
import Data.Ix
import Data.List

main = do
    grid <- fmap (S.fromList . concat . zipWith (\x cs ->
        map ((\y -> (x, y, 0)) . fst) . filter ((== '#') . snd) $ zip [0..] cs) [0..] . lines) getContents
    print $ S.size (iterate f grid !! 6)

f :: S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
f grid = S.filter active . S.fromList . concatMap neighs $ S.toList grid
    where neighs p@(x, y, z) = delete p $ range ((x-1, y-1, z-1), (x+1, y+1, z+1))
          active p = let n = length (take 4 . filter (`S.member` grid) $ neighs p) in n == 3 || (n == 2 && p `S.member` grid)