-- 2016-04-15
import qualified Data.Vector as Vec
import Data.Functor

main = do
  t <- read <$> getLine
  sequence_ (replicate t f)

m = 10^9 + 7
maxN = 2 * 10^6

facs :: Vec.Vector Integer
facs = Vec.fromList (1 : xs)
  where xs = (1 : zipWith (\x y -> (x * y) `mod` m) xs [2..maxN])

fac :: Int -> Integer
fac = (facs Vec.!)

inv :: Integer -> Integer
inv x = y where (_, y) = gcd' m x

gcd' :: Integer -> Integer -> (Integer, Integer)
gcd' _ 1 = (0, 1)
gcd' a b =
  let (n, m') = a `quotRem` b
      (xb, xm) = gcd' b m'
  in (xm, (xb - n * xm) `mod` m)

f :: IO ()
f = do
  (strN,  ' ' : strK) <- span (/= ' ') <$> getLine
  let (n, k) = (read strN, read strK)
  print $ choose (n - 1) (k - 1)

choose :: Int -> Int -> Integer
choose n k = ((fac n) * (inv . fac $ k) * (inv . fac $ (n - k))) `mod` m