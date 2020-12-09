main = do  
    ans <- length . filter isValid . lines <$> getContents :: IO Int
    print ans

isValid :: String -> Bool
isValid line = read n <= k && k <= read m
    where
        [n, m, c, p] = words $ map f line
        k = length $ filter (== head c) p

f :: Char -> Char
f '-' = ' '
f ':' = ' '
f c = c