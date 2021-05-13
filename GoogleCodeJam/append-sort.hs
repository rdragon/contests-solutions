-- This solution was written on 2021-05-09 during a practice round.

main :: IO ()
main = do
    testCount <- read <$> getLine
    sequence_ [runTest i | i <- [1..testCount]]

runTest :: Int -> IO ()
runTest testNumber = do
    _ <- getLine
    xs <- map read . words <$> getLine :: IO [Integer]
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++ show (f xs)

f :: [Integer] -> Int
f [_] = 0
f (x:y:xs)
    | y > x = f (y:xs)
    | y /= x' || allNines = let n = lenX - lenY + (if y <= x' then 1 else 0) in n + f ((y * 10 ^ n):xs)
    | otherwise = lenX - lenY + f ((x + 1):xs)
    where
        lenX = length $ show x
        lenY = length $ show y
        x' = read . take lenY $ show x :: Integer
        allNines = all (== '9') . drop lenY $ show x