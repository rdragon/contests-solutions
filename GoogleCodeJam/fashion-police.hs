-- 2016-06-22
-- voor deze oplossing de analyse gelezen tot "... so if we can find a set of that size, we are done!"
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad

main :: IO ()
main = do
  ints <- intReader
  [testc] <- ints 1
  forM_ [1..testc] $ \testn -> do
    [j, p, s, k'] <- ints 4
    let k = min s k'
    putStrLn $ "Case #" ++ show testn ++ ": " ++ show (j * p * k)
    let js = concatMap (replicate (p * k)) [1..j]
    let ps = concat . replicate j $ concatMap (replicate k) [1..p]
    let add x y = (x + y - 1) `mod` (max p k) + 1
    let t1 = concat . map (\(i, xs) -> map (add i) xs) $ zip [1..p] (repeat [1..k])
    let ss = concat . map (\(i, xs) -> map (add i) xs) $ zip [1..j] (repeat t1)
    putStrLn $ intercalate "\n" . map (intercalate " " . map show) $ zipWith3 (\x y z -> [x, y, z]) js ps ss

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
