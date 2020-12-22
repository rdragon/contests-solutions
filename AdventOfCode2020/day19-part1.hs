import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString.Char8 as C

main = do
    (ss, ts) <- fmap (span (not . null) . lines) getContents
    let set = S.fromList . map C.pack $ f (M.fromList $ map (\s -> let (t, u) = span (/= ':') s in (t, tail u)) ss) "0"
    print . length . filter ((`S.member` set) . C.pack) $ tail ts

f :: M.Map String String -> String -> [String]
f _ ('"':c:_) = [[c]]
f m i = concatMap (map concat . mapM (f m) . words) . g $ m M.! i
    where g [] = []
          g s = let (t, u) = span (/= '|') s in (t : g (drop 1 u))