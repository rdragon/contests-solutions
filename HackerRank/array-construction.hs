-- 2016-11-12
-- array-construction, university codesprint

import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as V
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed.Mutable as M

type Flag = (Int, Int, Int)
type S = M.IOVector Bool

len :: Int
len = 51 * 201 * 2001

main :: IO ()
main = do
  rd <- intReader
  [q] <- rd 1
  mp_ref <- newIORef (Map.empty)
  replicateM_ q $ do
    [n, s, k] <- rd 3
    mp <- readIORef mp_ref
    v <- case Map.lookup n mp of
      Nothing -> M.replicate len False
      Just x -> return x
    ans <- solve v n s k
    putStrLn . unwords . map show $ ans

solve :: S -> Int -> Int -> Int -> IO [Int]
solve v n s k
  | (odd n && odd k) || (even n && (even s /= even k)) = return [-1]
  | s == 0 = return $ if k == 0 then replicate n 0 else [-1]
  | otherwise = do
    mb <- run v n sumv s k (n - 1)
    return $ fromMaybe [-1] mb
  where
  sumv = V.fromList $ scanl1 (+) [n - 1, n - 3 .. 1 - n]

getKey :: Flag -> Int
getKey (s, k, i) = k * 201 * 51 + s * 51 + i

checkFlag :: S -> Flag -> IO Bool
checkFlag v x = M.read v (getKey x)

setFlag :: S -> Flag -> IO ()
setFlag v x = M.write v (getKey x) True

run :: S -> Int -> V.Vector Int -> Int -> Int -> Int -> IO (Maybe [Int])
run v n sumv s k i = do
  is_set <- checkFlag v (s, k, i)
  if is_set then return Nothing else do
    mb <- if i > 0 then run v n sumv s k (i - 1) else return Nothing
    case mb of
      Nothing -> try1
      Just xs -> return (Just xs)
  where
  make = replicate (n - 1 - i) 0 ++ replicate (i + 1) 1
  s' = s - (i + 1)
  k' = k - (sumv V.! i)
  try1 
    | s' == 0 = return $ if k' == 0 then (Just make) else Nothing
    | s' < 0 || k' < 0 = return Nothing
    | otherwise = try2
  try2 = do
    mb <- run v n sumv s' k' i
    case mb of
      Nothing -> do
        setFlag v (s, k, i)
        return Nothing
      Just xs -> return (Just (zipWith (+) make xs))

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words). B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n. map (fst. fromJust. B.readInt) $ xs)