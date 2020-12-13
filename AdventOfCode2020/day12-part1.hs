import Data.List

main = do
    (x, y, _) <- fmap (foldl' f (0, 0, 0) . lines) getContents
    print $ abs x + abs y

f :: (Int, Int, Int) -> String -> (Int, Int, Int)
f (x, y, d) (c:s)
    | c == 'N' = (x, y + n, d)
    | c == 'S' = (x, y - n, d)
    | c == 'E' = (x + n, y, d)
    | c == 'W' = (x - n, y, d)
    | c == 'L' = (x, y, (d + n) `mod` 360)
    | c == 'R' = (x, y, (d - n) `mod` 360)
    | d == 0 = (x + n, y, d)
    | d == 90 = (x, y + n, d)
    | d == 180 = (x - n, y, d)
    | d == 270 = (x, y - n, d)
    | otherwise = undefined
    where n = read s