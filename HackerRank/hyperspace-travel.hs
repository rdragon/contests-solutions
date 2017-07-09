-- 2016-09-02
-- hyperspace travel, ad infinitum 16
import Data.List
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [n, m] <- rd 2
  xss <- fmap transpose $ replicateM n $ rd m
  putStrLn $ unwords $ map (show . solve) xss

solve :: [Int] -> Int
solve xs = sort xs !! ((length xs - 1) `quot` 2)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)