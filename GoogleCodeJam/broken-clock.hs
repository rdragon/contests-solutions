-- This solution was written on 2021-05-16 during a practice round.

import Data.List
import Data.Maybe

main :: IO ()
main = do
    n <- read <$> getLine
    sequence_ [runTest i | i <- [1..n]]

runTest :: Int -> IO ()
runTest i = do
    xs <- solve . map read . words <$> getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ (intercalate " " (map show xs))

solve :: [Integer] -> [Integer]
solve xs = head $ catMaybes [trySolve a b c | [a, b, c] <- permutations xs]

trySolve :: Integer -> Integer -> Integer -> Maybe [Integer]
trySolve a b c = if ((c' - c) `mod` n == 0) then Just [hours, minutes, seconds, nanoseconds] else Nothing
    where
        n = 360 * 12 * 10 ^ 10
        r = (12 * a - b) * 15709090909091
        c' = 720 * (a - r) + r
        t = (a - r) `mod` n
        (hours, remainder) = t `quotRem` (60 * 60 * 10 ^ 9)
        (minutes, remainder') = remainder `quotRem` (60 * 10 ^ 9)
        (seconds, nanoseconds) = remainder' `quotRem` (10 ^ 9)
