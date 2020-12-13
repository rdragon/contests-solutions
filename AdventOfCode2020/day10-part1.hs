import Data.List

main = do
    (a, b, _) <- fmap (foldl' f (0, 0, 0) . sort . map read . lines) getContents
    print $ a * succ b

f :: (Int, Int, Int) -> Int -> (Int, Int, Int)
f (a, b, x) y = if succ x == y then (succ a, b, y) else (a, succ b, y)