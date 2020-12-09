main = do  
    xs <- map read . lines <$> getContents :: IO [Int]
    print $ head [x * y | x <- xs, y <- xs, x + y == 2020]