import Data.List

main = do
    getLine
    ids <- fmap (filter ((/= 0) . snd) . zip [0..] . map read . words . map f) getLine
    print . fst $ foldl' g (0, 1) ids

g :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
g (a, n) (r, p) = (a + n * j, n * p)
    where j = head [i | i <- [0..], (a + n * i) `mod` p == (p - r) `mod` p]

f :: Char -> Char
f ',' = ' '
f 'x' = '0'
f c = c