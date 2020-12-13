import Data.List

main = do
    n <- fmap read getLine :: IO Int
    ids <- fmap (map read . words . map f) getLine
    let (x, y) = head $ sortOn snd [(i, i - n `mod` i) | i <- ids]
    print $ x * y

f :: Char -> Char
f ',' = ' '
f 'x' = ' '
f c = c