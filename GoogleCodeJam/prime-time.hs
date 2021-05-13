-- This solution was written on 2021-05-09 during a practice round.

import Data.Int
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
    testCount <- read <$> getLine
    sequence_ [runTest i | i <- [1..testCount]]

primes :: [Int64]
primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499]

runTest :: Int -> IO ()
runTest testNumber = do
    m <- read <$> getLine :: IO Int
    pns <- sequence $ replicate m (map read . words <$> getLine) :: IO [[Int64]]
    putStrLn $ "Case #" ++ show testNumber ++ ": " ++ show (f pns)

f :: [[Int64]] -> Int64
f pns = head . catMaybes $ [g pns m i | i <- [2..3500]] ++ [Just 0]
  where
      m = sum [p * n | [p, n] <- pns]

g :: [[Int64]] -> Int64 -> Int64 -> Maybe Int64
g pns m i
    | m' < 2 = Nothing
    | isNothing qs' = Nothing
    | i /= sum qs = Nothing
    | otherwise = if hasPrimes pns qs then Just m' else Nothing
    where
        m' = m - i
        qs' = factorize m'
        qs = fromJust qs'

hasPrimes :: [[Int64]] -> [Int64] -> Bool
hasPrimes _ [] = True
hasPrimes [] _ = False
hasPrimes ([p, n]:pns) (q:qs)
    | q < p = False
    | q == p = hasPrimes (if n == 1 then pns else ([p, n - 1]:pns)) qs
    | otherwise = hasPrimes pns (q:qs)

factorize :: Int64 -> Maybe [Int64]
factorize m 
    | m `elem` primes = Just [m]
    | otherwise = join $ listToMaybe [(p:) <$> factorize (m `quot` p) | p <- primes, m `mod` p == 0]
