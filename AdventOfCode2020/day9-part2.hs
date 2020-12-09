import Data.List
import Control.Monad

main = do 
    numbers <- fmap (map read . lines) getContents
    print $ compute numbers

compute :: [Int] -> Int
compute numbers = head [maximum ws + minimum ws | ws <- zs, sum ws == invalidNumber]
    where
        invalidNumber =
            let xs = drop 26 . reverse . map (take 26) . tails $ reverse numbers
            in head [y | (y:ys) <- xs, null [x | x <- ys, x' <- ys, x /= x' && x + x' == y]]
        zs = join . map (take 1 . reverse . takeWhile ((<= invalidNumber) . sum) . drop 2 . inits) $ tails numbers