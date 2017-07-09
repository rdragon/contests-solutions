-- 2016-08-27
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad
import Data.List

main :: IO ()
main = do
  rd <- intReader
  [t] <- rd 1
  replicateM_ t $ do
    [n, k, b] <- fmap (map fromIntegral) $ rd 3 :: IO [Integer]
    let
      p = (2 * n + b * b - b + 2 * b - 1) `quot` (2 * b)
      d = f p b - n
      vals = [p - b + 1 .. p]
      ans
        | d == 0 = vals
        | otherwise = let (x, y) = splitAt (fromIntegral d - 1) vals in (p - b) : (x ++ tail y)
    putStrLn $ intercalate " " $ map show $ if n < g b || p > k then [-1] else ans
  where
  f p b = g p - g (p - b)
  g p = (1 + p) * p `quot` 2

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)