-- 2016-09-17
-- enclosure, week of code 23
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  ls <- fmap (map fromIntegral) $ rd n
  let
    radius = g rMin rMax 100
      where
      rMin = sum ls' / pi
      rMax = rMin * 1.6
      g :: Double -> Double -> Int -> Double
      g a b i
        | i == 0 = c
        | f c < pi = g a c (pred i)
        | otherwise = g c b (pred i)
        where
        c = (a + b) / 2
      f :: Double -> Double
      f = sum . h
      h r = map (\x -> if x > r then 2 * pi else asin $ x / r) ls'
    ls' = map (/ 2) ls
    s = unlines $ concatMap f angles
      where
      angles = let as = h radius in scanl (-) (pi + head as) $ map (* 2) $ init as
      cx = let l = head ls in sqrt $ radius * radius - l * l / 4
      cy = head ls'
      f a = [show x, show y, ""]
        where
        x = cx + radius * cos a
        y = cy + radius * sin a
      h r = map (\x -> asin $ min 1 $ x / r) ls'
  putStrLn s

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
