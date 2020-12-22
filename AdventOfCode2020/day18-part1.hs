import Data.Char

main = do
    xs <- fmap (map (fst . f . filter (/= ' ')) . lines) getContents
    print (sum xs)

f :: String -> (Integer, String)
f s = case t of [] -> (n, [])
                (')':u) -> (n, u)
                ('+':u) -> h (+) u
                ('*':u) -> h (*) u
      where (n, t) = g s
            h p u = let (m, v) = g u in f $ (show $ p n m) ++ v
            g ('(':u) = f u
            g u = let (v, w) = span isDigit u in (read v, w)
