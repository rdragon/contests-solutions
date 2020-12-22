import qualified Data.Map.Strict as M

main = do
    (ss, ts) <- fmap (span (not . null) . lines) getContents
    let m = M.fromList $ map (\s -> let (t, u) = span (/= ':') s in (t, tail u)) ss
        [p, q] = map (flip elem . f m) ["42", "31"]
        g s = let (t, u) = span q . reverse $ h s; n = length t in n >= 1 && length u >= n + 1 && all p u
        h [] = []
        h s = let (t, u) = splitAt 8 s in (t : h u)
    print . length . filter g $ tail ts

f :: M.Map String String -> String -> [String]
f _ ('"':c:_) = [[c]]
f m i = concatMap (map concat . mapM (f m) . words) . g $ m M.! i
    where g [] = []
          g s = let (t, u) = span (/= '|') s in (t : g (drop 1 u))