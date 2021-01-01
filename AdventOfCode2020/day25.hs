import Data.List
import Data.Int

main = do
    [p, q] <- (map read . lines) <$> getContents
    print $ p `pow` root q

root :: Int64 -> Int
root x = length . takeWhile (/= x) $ iterate (mult 7) 1

mult :: Int64 -> Int64 -> Int64
mult x = (`mod` 20201227) . (* x)

pow :: Int64 -> Int -> Int64
pow x y = iterate' (mult x) 1 !! y