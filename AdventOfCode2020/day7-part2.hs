import Data.List

main = do  
    f' <- fmap (f1 . map f . lines) getContents
    print $ pred $ h f' "shiny gold"

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

h :: (String -> [(Int, String)]) -> String -> Int
h f' s = succ $ sum [i * h f' s' | (i, s') <- f' s]

f1 :: [(String, [(Int, String)])] -> String -> [(Int, String)]
f1 xs s = head [ys | (t, ys) <- xs, t == s]