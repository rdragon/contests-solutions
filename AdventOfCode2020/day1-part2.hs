main = do  
    xs <- map read . lines <$> getContents :: IO [Int]
    print $ head [x * y * z | x <- xs, y <- xs, z <- xs, x + y + z == 2020]