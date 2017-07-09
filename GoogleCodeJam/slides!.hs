-- 2016-06-21
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad

main :: IO ()
main = do
  ints <- intReader
  [t] <- ints 1
  forM_ [1..t] $ \i -> do
    [b, m] <- ints 2
    let x = solve b m
    putStr $ "Case #" ++ show i ++ ": "
    putStrLn $ maybe "IMPOSSIBLE" (intercalate "\n" . ("POSSIBLE" :)) x

solve :: Int -> Int -> Maybe [String]
solve b m
  | last cs < m = Nothing
  | last cs == m = Just $ bs ++ [c] ++ ds
  | otherwise = Just $ [a] ++ tail bs ++ [c] ++ ds
  where
  cs = takeWhile (< 2 * m) . take (pred b) . iterate (* 2) $ 1
  n = length cs + 1
  a = take b . (++ repeat '0') . ('0' :) . reverse $ '1' : f (m - 1)
  bs = take (n - 1) . map (take b) . map (\x -> replicate x '0' ++ replicate (n - x) '1' ++ repeat '0') $ [1..]
  c = if null ds then replicate b '0' else replicate (pred b) '0' ++ ['1']
  ds = replicate (b - n) (replicate b '0')
  f 0 = []
  f x
    | y == 0 = '0' : f z
    | otherwise = '1' : f z
    where
    (z, y) = x `divMod` 2

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
