-- 2016-09-02
-- divisor exploration, ad infinitum 16
{-# LANGUAGE BangPatterns #-}
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V

main :: IO ()
main = do
  rd <- intReader
  [d] <- rd 1
  let
    !facs = V.fromList $ take 200002 $ map fst $ iterate f (1, 1)
      where
      f (a, b) = (mult a (mult b b), succ b)
  replicateM_ d $ do
    [m, a] <- rd 2
    print $ solve m a facs

solve :: Int -> Int -> V.Vector Int -> Int
solve m a facs = (a + 2) `mult` (a + 2 + m) `mult` inv (pow 2 m `mult` fac (a + 2)) `mult` fac (a + 1 + m)
  where
    fac = (facs V.!)

modulo :: Int
modulo = 1000000007

mult :: Int -> Int -> Int
mult !x !y = (x * y) `mod` modulo

inv :: Int -> Int
inv x = pow x (modulo - 2)

pow :: Int -> Int -> Int
pow = go 1
  where
  go x _ 0 = x
  go x a b = go y (mult a a) q
    where
    (q, r) = b `divMod` 2
    y = if r == 0 then x else mult x a

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)