import Data.List

main = do
    xs <- fmap (sort . map read . lines) getContents
    print . snd . head $ foldr f [(last xs, 1)] (0 : init xs)

f :: Int -> [(Int, Integer)] -> [(Int, Integer)]
f x ps = let m = sum [n | (y, n) <- ps, y <= x + 3]
         in ((x, m) : take 2 ps)