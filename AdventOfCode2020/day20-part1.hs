import Data.List
import qualified Data.Map.Strict as M

main = do
    tiles <- (f . lines) <$> getContents
    let m = M.filter (> 1) . foldl' (\n k -> M.insertWith (+) k 1 n) M.empty $ concatMap snd tiles
    print . foldl1' (*) . map fst $ filter ((==2) . length . filter (`M.member` m) . snd) tiles

f :: [String] -> [(Integer, [String])]
f [] = []
f ss = ((read . init . drop 5 $ head ts, g $ tail ts) : f (drop 1 us))
    where (ts, us) = span (not . null) ss
          g s = (\s -> min s $ reverse s) <$> ([head, last] <*> [s, transpose s])