-- 2016-12-01
-- music-on-the-street, week of code 26
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  as <- rd n
  [m, h0, h1] <- rd 3
  let as' = [head as - h1] ++ as ++ [last as + h1]
  print $ if m <= h1 then head as' else solve' m h0 h1 as'
  
solve' :: Int -> Int -> Int -> [Int] -> Int
solve' m h0 h1 (a:b:xs)
  | b - a < h0 = solve' m h0 h1 (b:xs)
  | otherwise = solve m h0 h1 (Seq.fromList [max a (b - h1), b]) 0 xs
solve' _ _ _ x = error ("x = " ++ show x)
  
solve :: Int -> Int -> Int -> Seq.Seq Int -> Int -> [Int] -> Int
solve m h0 h1 as l bs@(b:bs1)
  | b - a3 < h0 = solve' m h0 h1 bs
  | min b (a3 + h1) - a0 >= m && m - l >= 2 * h0 = max a0 (a3 + h0 - m)
  | min b (a3 + h1) - a0 >= m = solve m h0 h1 (Seq.drop 1 as) (l - a2 + a1) bs
  | b - a3 <= h1 = solve m h0 h1 (as Seq.|> b) (l + b - a3) bs1
  | otherwise = solve' m h0 h1 (a3:bs)
  where
  a0:a1:rest = Fold.toList as
  a2:_ = rest
  _ Seq.:> a3 = Seq.viewr as
solve _ _ _ _ _ [] = error "null bs"

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words). B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n. map (fst. fromJust. B.readInt) $ xs)