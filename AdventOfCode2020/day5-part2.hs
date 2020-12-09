main = do  
    ids <- map getId . lines <$> getContents :: IO [Int]
    print $ head [i | i <- [1..], pred i `elem` ids, i `notElem` ids]

getId :: String -> Int
getId s = let (t, u) = splitAt 7 s in 8 * getInt t + getInt u

getInt :: String -> Int
getInt s = f (reverse s, 1)

f :: (String, Int) -> Int
f ([], _) = 0
f ((c:cs), n) = (if c == 'B' || c == 'R' then n else 0) + f (cs, n * 2)