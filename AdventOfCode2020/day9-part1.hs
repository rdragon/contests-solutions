import Data.List

main = do  
    xs <- fmap (drop 26 . reverse . map (take 26) . tails . reverse . map read . lines) getContents :: IO [[Int]]
    print $ head [y | (y:ys) <- xs, null [x | x <- ys, x' <- ys, x /= x' && x + x' == y]]