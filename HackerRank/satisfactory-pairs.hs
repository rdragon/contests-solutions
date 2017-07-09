-- 2016-12-04
-- satisfactory-pairs, week of code 26
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as M

main :: IO ()
main = do
  n <- fmap read getLine
  solve n >>= print

solve :: Int -> IO Int
solve n = do
  mv <- M.replicate n False
  x <- alt n [a | a <- [k + 1.. n `quot` 2]] mv
  return $ x + sum [go a | a <- [1.. k]]
  where
  k = 4 * head [z | z <- [1..], z * z > n]
  go a = sum [inner a x | x <- [0.. a - 1]]
  inner a x
    | r /= 0 = 0
    | otherwise = count q (a `quot` c) (x `quot` c) i
    where
    (c, _, i) = egcd a x
    (q, r) = n `quotRem` c
    
alt :: Int -> [Int] -> M.IOVector Bool -> IO Int
alt n as mv = fmap sum $ sequence [go a | a <- as]
  where
  go :: Int -> IO Int
  go a = do
    bs <- fmap concat $ sequence [inner y | y <- [1.. m - 1]]
    sequence_ [M.write mv b False | b <- bs]
    return (length bs)
    where 
    m = pred n `quot` a
    inner :: Int -> IO [Int]
    inner y = fmap catMaybes $ sequence [super y k | k <- [1.. m - y]]
    super :: Int -> Int -> IO (Maybe Int)
    super y k
      | r == 0 = M.read mv b >>= fun
      | otherwise = return Nothing
      where
      (b, r) = (n - k * a) `quotRem` y
      fun :: Bool -> IO (Maybe Int)
      fun True = return Nothing
      fun False = do
        M.write mv b True
        return (Just b)
    
count :: Int -> Int -> Int -> Int -> Int
count n a x i = max 0 $ (pred n `quot` y - x) `quot` a - (if x == 0 then 1 else 0)
  where y = case (n * i) `mod` a of 0 -> a; z -> z
  
-- assumption: a, b >= 0
egcd :: Int -> Int -> (Int, Int, Int)
egcd a 0 = (a, 1, 0)
egcd a b = (c, n, m - n * q)
  where
  (c, m, n) = egcd b r
  (q, r) = a `divMod` b