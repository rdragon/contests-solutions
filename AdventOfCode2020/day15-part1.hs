import Data.List

main = do
    xs <- fmap (reverse . map read . words . map (\c -> if c == ',' then ' ' else c)) getContents
    print $ map head (iterate f xs) !! (2020 - length xs)

f :: [Int] -> [Int]
f xs@(x:xs') = case x `elemIndex` xs' of Just i -> (succ i : xs)
                                         Nothing -> (0 : xs)