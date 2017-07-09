-- 2016-06-21
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  f <- fmap (fmap fromIntegral) intReader
  [n, k] <- f 2
  xs <- f n
  let a = foldl' gcd (head xs) (tail xs)
  let ps = filter (<= k) . getFactors $ a
  let g p = (k `quot` p) * p
  let ys = 0 : map g ps
  print . maximum $ ys

getFactors :: Integer -> [Integer]
getFactors a0 = f a0 $ primes where
  f 1 _ = []
  f a (p:ps)
    | p * p > a = [a]
    | a `mod` p == 0 = let b = g a p in p : f b ps
    | otherwise = f a ps
  f _ _ = undefined
  g a p
    | a `mod` p == 0 = g (a `quot` p) p
    | otherwise = a

primes :: [Integer]
primes = 2 : 3 : filter f [5, 7..] where
  f n = all (\p -> n `mod` p /= 0) . takeWhile (\p -> p * p <= n) $ primes
  
intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
