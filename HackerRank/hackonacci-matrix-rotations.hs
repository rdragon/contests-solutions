-- 2016-12-21
-- hackonacci-matrix-rotations, week of code 27
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [n, angle_count] <- rd 2
  let
    compute b
      = length [ () 
               | i <- [1..n], j <- [1..n]
               , let
                   p = i * i * j * j
                   q = k * k * l * l
                   k = n + 1 - i
                   l = if b then n + 1 - j else j
               , f p /= f q ]
    x90 = compute False
    x180 = compute True
    solve 0 = 0
    solve 180 = x180
    solve _ = x90
  angles <- rd angle_count
  sequence_ [print (solve (x `mod` 360)) | x <- angles]
  where
  f p = let x = p `mod` 7 in x == 2 || x == 4 || x == 5

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words). B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n. map (fst. fromJust. B.readInt) $ xs)