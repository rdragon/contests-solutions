-- 2016-04-16
import Data.Functor

dot :: [Double] -> [Double] -> Double
dot v v' = sum (zipWith (*) v v')

len :: [Double] -> Double
len v = sqrt (dot v v)

main = do
  n <- read <$> getLine
  points <- sequence (replicate n getPoint)
  let angles = [angle (points !! i) (points !! j) (points !! k) | i <- [0..n-1], j <- [i+1..n-1], k <- [j+1..n-1]]
      n' = fromIntegral n
  print $ (6 / (n' * (n' - 1) * (n' - 2))) * sum angles

getPoint :: IO [Double]
getPoint = (fmap read . words) <$> getLine

angle :: [Double] -> [Double] -> [Double] -> Double
angle u v w = acos $ dot a b / len a / len b
  where a = zipWith (-) u v
        b = zipWith (-) w v