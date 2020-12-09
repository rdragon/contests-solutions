main = do  
    ans <- length . filter isValid . lines <$> getContents :: IO Int
    print ans

isValid :: String -> Bool
isValid line = (c1 == head c) /= (c2 == head c)
    where
        [n, m, c, p] = words $ map f line
        c1 = p !! (pred $ read n)
        c2 = p !! (pred $ read m)

f :: Char -> Char
f '-' = ' '
f ':' = ' '
f c = c