-- 2016-07-04
-- net iets te langzaam, maar zelfde algoritme als in editorial
import Data.List
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V

type Point = (Int, Int)

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  ps <- replicateM n $ fmap (\[x, y] -> (x, y)) $ rd 2
  ans <- solve ps
  print ans

solve :: [Point] -> IO Int
solve ps = do
  cs1 <- getCounts ps
  cs2 <- getCounts $ map (\(x, y) -> (-x, -y)) ps
  return $ sum [getC cs1 b c * getC cs2 c b | b <- [0..pred n], c <- [0..pred n]]
  where
  getC cs b c = cs V.! (c * n + b)
  n = length ps

getCounts :: [Point] -> IO (V.Vector Int)
getCounts ps = do
  cs <- M.replicate (n * n) 0
  forM_ (zip ps [0..]) $ \(cp, c) -> do
    let
      go [] _ _ _ = return ()
      go ((r, b) : rs2) rs1 rs5 m = do
        M.write cs (c * n + b) m1
        go rs2 rs4 rs7 m1
        where
        (rs3, rs4) = span ((> pi / 2 + eps) . (r -) . fst) rs1
        m1 = m - length rs3 + length rs6
        (rs6, rs7) = span ((< (r - eps)) . fst) rs5
    let rs = sort $ filter (f c) $ zip (map (getAngle cp) ps) [0..]
        (rs1, rs2) = span ((< -eps) . fst) rs
        in go rs2 rs rs2 (length rs1)
  V.freeze cs
  where
  f c (r, b) = not (b == c || r > pi - eps)
  getAngle (cx, cy) (bx, by)
    | x == 0 = if y > 0 then pi / 2 else -pi / 2
    | otherwise = atan (fromIntegral y / fromIntegral x) + (if x > 0 then 0 else pi)
    where
    x = bx - cx
    y = by - cy
  n = length ps

eps :: Double
eps = 0.0000000001

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)