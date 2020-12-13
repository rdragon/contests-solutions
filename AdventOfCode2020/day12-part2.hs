import Data.List

main = do
    (x, y, _, _) <- fmap (foldl' f (0, 0, 10, 1) . lines) getContents
    print $ abs x + abs y

f :: (Int, Int, Int, Int) -> String -> (Int, Int, Int, Int)
f (x, y, wx, wy) (c:s)
    | c == 'N' = (x, y, wx, wy + n)
    | c == 'S' = (x, y, wx, wy - n)
    | c == 'E' = (x, y, wx + n, wy)
    | c == 'W' = (x, y, wx - n, wy)
    | c == 'F' = (x + wx * n, y + wy * n, wx, wy)
    | d == 90 = (x, y, -wy, wx)
    | d == 180 = (x, y, -wx, -wy)
    | d == 270 = (x, y, wy, -wx)
    | otherwise = undefined
    where n = read s
          d = (if c == 'L' then n else -n) `mod` 360