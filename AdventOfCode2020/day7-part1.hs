import Data.List

main = do  
    f' <- fmap (f1 . map f . lines) getContents
    print $ pred $ length (h f' ([], ["shiny gold"]))

f :: String -> (String, [(Int, String)])
f s | " bags contain " `isPrefixOf` s = ([], g $ drop 14 s)
f (c:cs) = let (s, ss) = f cs in (c:s, ss)

g :: String -> [(Int, String)]
g "no other bags." = []
g s = ((read t, v) : rest)
    where
        (t, u) = span (/= ' ') s
        (v', w) = span (`notElem` ".,") $ tail u
        v = reverse . tail . dropWhile (/= ' ') $ reverse v'
        rest = if w == "." then [] else g $ drop 2 w

h :: (String -> [String]) -> ([String], [String]) -> [String]
h _ (ss, []) = ss
h f' (ss, t:ts)
    | t `elem` ss = h f' (ss, ts)
    | otherwise = h f' ((t:ss), ts ++ f' t)

f1 :: [(String, [(Int, String)])] -> String -> [String]
f1 xs s = [t | (t, ys) <- xs, s `elem` (map snd ys)]