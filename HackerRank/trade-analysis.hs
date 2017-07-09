-- 2016-06-29
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  ws <- readWords
  [n] <- readInts ws 1
  ts <- readInts ws n
  print (solve 0 0 ts)

md :: Int
md = 1000000007

solve :: Int -> Int -> [Int] -> Int
solve _ b [] = b
solve a b (t:ts) = solve a' b' ts
  where
    a' = (a + prod t (succ a)) `mod` md
    b' = (b + prod t (a + succ b)) `mod` md

prod :: Int -> Int -> Int
prod x y = (x * y) `mod` md

readWords :: IO (IORef [B.ByteString])
readWords = fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef

readInts :: IORef [B.ByteString] -> Int -> IO [Int]
readInts ref n = do
  x <- readIORef ref
  writeIORef ref (drop n x)
  return (take n . map (fst . fromJust . B.readInt) $ x)