import Data.List
import qualified Data.Set as S

main = interact $ show . length . (!! 100) . iterate run . S.fromList . map head . filter f . group . sort . map walk . lines
    where f = (== 1) . (`mod` 2) . length

walk :: String -> (Int, Int)
walk s = foldl1' f . tail . map snd . takeWhile (not . null . fst) $ iterate (g . fst) (s ++ "e", undefined)
    where f (a, b) (c, d) = (a + c, b + d)
          g ('e':s)     = (s, (1, 0))
          g ('s':'e':s) = (s, (1, -1))
          g ('s':'w':s) = (s, (0, -1))
          g ('w':s)     = (s, (-1, 0))
          g ('n':'w':s) = (s, (-1, 1))
          g ('n':'e':s) = (s, (0, 1))

run :: S.Set (Int, Int) -> S.Set (Int, Int)
run u = S.filter f . S.fromList . concatMap g $ S.toList u
    where f p | p `S.member` u = n == 1 || n == 2
              | otherwise = n == 2
              where n = length . filter (`S.member` u) $ g p
          g (x, y) = [(x + 1, y), (x + 1, y - 1), (x, y - 1), (x - 1, y), (x - 1, y + 1), (x, y + 1)]