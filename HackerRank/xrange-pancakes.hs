-- 2016-09-02
-- xrange's pancakes, ad infinitum 16
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad

main :: IO ()
main = do
  rd <- intReader
  [n, m] <- rd 2
  cooks <- replicateM m $ rd 2
  putStrLn $ solve n cooks

solve :: Int -> [[Int]] -> String
solve n = go (False, 0)
  where
  go (False, i) [] = "1 " ++ show i
  go (True, i) [] = "2 " ++ show i
  go (fl, i) ([1, j] : rest) = go (fl, (if fl then i + j else i - j) `mod` n) rest
  go (fl, i) ([2, j] : rest) = go (not fl, (if fl then i - j else j + i) `mod` n) rest

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
