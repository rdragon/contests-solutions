import Data.Char
import Data.List

main = do
    ans <- fmap (f . lines . map (\c -> if isDigit c || c == '\n' then c else ' ')) getContents
    print ans

f :: [String] -> Int
f ss = sum . filter (not . isValid (concat ts)) . concat $ drop 5 rs
    where (ts, rs) = span (not . null) $ map (map read . words) ss

isValid :: [Int] -> Int -> Bool
isValid (y:z:xs) x = (y <= x && x <= z) || isValid xs x
isValid _ _ = False