-- This solution was written on 2021-05-09 during a practice round.
-- This solution only solves test sets one and two as it cannot handle more than two exams.

main :: IO ()
main = do
    testCount <- read <$> getLine
    sequence_ [runTest i | i <- [1..testCount]]

runTest :: Int -> IO ()
runTest testNumber = do
    [n, q] <- map read . words <$> getLine :: IO [Int]
    ass <- sequence $ replicate n $ (\[a, s'] -> inverse' q (a, read s')). words <$> getLine :: IO [(String, Int)]
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++ (f q ass)

f :: Int -> [(String, Int)] -> String
f q [(a, s)] = a ++ " " ++ show s ++ "/1"
f q [(a, s), (b, t)] = f q (if s > t then [(a, s)] else [(b, t)])
f q ((a, s):_) = f q [(a, s)]

inverse :: String -> String
inverse = map (\c -> if c == 'T' then 'F' else 'T')

inverse' :: Int -> (String, Int) -> (String, Int)
inverse' q (a, s) = if s * 2 >= q then (a, s) else (inverse a, q - s)