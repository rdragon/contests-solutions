-- 2016-11-10
-- kindergarten-adventures, university codesprint
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.IntMap.Strict as Map

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  ts <- rd n
  print (solve n ts)

solve :: Int -> [Int] -> Int
solve n ts = succ . fromJust $ findIndex (== mx) counts
  where
  count = length . filter (\(t, i) -> t <= i) . zip ts $ [0..]
  mp = foldr (Map.alter succ') Map.empty (filter (>= 0) . zipWith (\t i -> i - t) ts $ [0..])
  mx = maximum counts
  counts = take n $ run n count mp (cycle ts) 0

succ' :: Maybe Int -> Maybe Int
succ' Nothing = Just 1
succ' x = fmap succ x

run :: Int -> Int -> Map.IntMap Int -> [Int] -> Int -> [Int]
run n c mp (t:ts) t0 = c : run n c2 mp2 ts (t0 + 1)
  where
  (c1, mp1) = case Map.lookup t0 mp of
    Nothing -> (c, mp)
    Just x -> (c - x, Map.delete t0 mp)
  (c2, mp2)
    | t == n = (c1, mp1)
    | otherwise = (c1 + 1, Map.alter succ' (n - 1 - t + t0 + 1) mp1)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words). B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n. map (fst. fromJust. B.readInt) $ xs)