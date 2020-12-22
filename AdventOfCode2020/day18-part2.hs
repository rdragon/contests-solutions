import Data.Char

main = do
    xs <- fmap (map (fst . (f True) . filter (/= ' ')) . lines) getContents
    print (sum xs)

f :: Bool -> String -> (Integer, String)
f b s = case t of [] -> (n, [])
                  (')':u) -> (n, if b then u else t)
                  ('+':u) -> h (+) $ g u
                  ('*':u) -> if b then h (*) $ f False u else (n, t)
         where (n, t) = g s
               g ('(':u) = f True u
               g u = let (v, w) = span isDigit u in (read v, w)
               h p (m, v) = f b $ show (p n m) ++ v