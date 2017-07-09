-- 2016-06-29
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  x <- readWords
  n <- readInt x
  rs <- readInts x n
  ms <- readInts x n
  print $ solve rs ms

solve :: [Int] -> [Int] -> Int
solve rs ms = sum . map fst . map snd . Map.toList $ mp0
  where
    n = length rs
    mp0 = foldl' f Map.empty (sort $ zip3 rs [0..] ms)
    f mp (r, i, m) = Map.insert i (x, r) mp
      where
        x = maximum . (m :) . map succ . map fst . filter ((< r) . snd) . map fromJust . filter isJust . map (flip Map.lookup mp) $ [max 0 (i - 10) .. min (pred n) (i + 10)]
    

readWords :: IO (IORef [B.ByteString])
readWords = fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef

readInt :: IORef [B.ByteString] -> IO Int
readInt = fmap head . flip readInts 1

readInts :: IORef [B.ByteString] -> Int -> IO [Int]
readInts ref n = do
  x <- readIORef ref
  writeIORef ref (drop n x)
  return (take n . map (fst . fromJust . B.readInt) $ x)