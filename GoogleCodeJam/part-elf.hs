-- 2016-09-14
-- part elf, code jam 2014
import Data.Maybe
import Data.List
import Data.Bits
import Control.Monad

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

solveM :: IO String
solveM = do
  s <- getLine
  let
    i = fromJust $ '/' `elemIndex` s
    (a, (_:b)) = splitAt i s
    p = read a
    q = read b
    r = gcd p q
  return $ solve (p `quot` r) (q `quot` r)

solve :: Int -> Int -> String
solve p q
  | not $ isPowerOfTwo q = "impossible"
  | otherwise = show $ head $ dropWhile f [1..]
  where
  f i = p * r' < q'
    where
    r = 1 `shift` i
    s = gcd q r
    r' = r `quot` s
    q' = q `quot` s

isPowerOfTwo :: Int -> Bool
isPowerOfTwo 0 = False
isPowerOfTwo n = n == (n .&. (-n))
