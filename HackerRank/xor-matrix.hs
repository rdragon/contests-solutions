-- 2016-10-14
-- xor matrix, week of code 24
{-# LANGUAGE BangPatterns #-}
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import qualified Data.Vector.Unboxed as V
import Data.Bits

main :: IO ()
main = do
  rd <- intReader
  [n, m] <- rd 2
  s <- unwords . map show . V.toList . go n m . V.fromList <$> rd n
  putStrLn s
  --let n = 100000; m = 1000000000000000000; s = sum . V.toList . go n m . V.fromList $ [1..n]
  --print s
  where
  go _ 1 !v = v
  go n m !v = go n (m - k + 1) w
    where
    k = last . takeWhile (<= m) . map succ . iterate (*2) $ 1
    w = V.generate n (\i -> (v V.! i) `xor` (v V.! ((i + k - 1) `mod` n)))

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)