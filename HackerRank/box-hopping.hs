-- 2016-04-20
import Data.List

runPlayer :: [Int] -> Int -> Int -> Int -> Bool
runPlayer xs n i 0 = if xs !! i == n then True else False
runPlayer xs n i j = if xs !! i == n then True else runPlayer xs n (xs !! i) (j - 1)

nIn = 8
kIn = 3

main = print $ length $ filter ok [bs | bs <- map (0:) $ permutations [1..nIn]]
  where
    ok :: [Int] -> Bool
    ok bs = and [runPlayer bs n n (kIn - 1) | n <- [1..nIn]]
    
    
    
    
    
    
    
import qualified Data.Vector as Vec

nMax :: Int
nMax = 1000

v :: Vec.Vector Double
v = Vec.generate (nMax * nMax) gen

gen :: Int -> Double
gen i
  = fromIntegral n * f k (n - 1) - product [fromIntegral (n - j) | j <- [1..k]] * f k (n - 1 - k)
  where
    k = (i `quot` nMax) + 1
    n = (i `mod` nMax) + 2
    
f :: Int -> Int -> Double
f k n
  | n <= k = fac n
  | otherwise = v Vec.! (n - 2 + (k - 1) * nMax)

facs :: Vec.Vector Double
facs = Vec.fromList (1 : xs)
  where xs = (1 : zipWith (*) xs [2..fromIntegral nMax])

fac :: Int -> Double
fac = (facs Vec.!)

main = do
  t <- fmap read getLine
  sequence_ $ replicate t $ do
    (n, k) <- readPair
    print $ f k n / fac n
  where
    readPair = do
      (s1,  ' ' : s2) <- fmap (span (/= ' ')) getLine
      return (read s1, read s2)
  