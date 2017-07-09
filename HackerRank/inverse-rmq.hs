-- 2016-09-26
-- inverse rmq, world codesprint 7
import Control.Monad.Trans.Class
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Set as Set
import Control.Monad.Reader

type A = M.IOVector (Set.Set Iv)
type B = M.IOVector Int
type Env = (A, B)
type S = ReaderT Env IO
type Iv = (Int, Int)

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  xs <- fmap (map (\ys -> (head ys, length ys)) . group . sort) $ rd (2 * n - 1)
  let k = succ $ lg n
  v <- M.replicate (succ k) Set.empty
  w <- M.new n
  M.write v k (Set.singleton (0, pred n))
  when (maximum (map snd xs) == k) $ runReaderT (run xs) (v, w)
  ok <- fmap (all Set.null . V.toList) $ V.freeze v
  s <- fmap (unwords . map show . reverse . mkSegTree . reverse . V.toList) $ V.freeze w
  putStrLn $ if ok then "YES\n" ++ s else "NO"

lg :: Int -> Int
lg 1 = 0
lg n = succ $ lg (n `quot` 2)

mkSegTree :: [Int] -> [Int]
mkSegTree [x] = [x]
mkSegTree xs = xs ++ mkSegTree (f xs)
  where
  f [] = []
  f (x:y:ys) = (min x y) : f ys

run :: [(Int, Int)] -> S ()
run [] = return ()
run ((p, k) : rest) = do
  i_mb <- getIx k
  if isNothing i_mb then return () else do
  let i = fromJust i_mb
  writeW i p
  run rest

getV :: S A
getV = asks fst

getW :: S B
getW = asks snd

getIx :: Int -> S (Maybe Int)
getIx k = do
  v <- getV
  st <- lift $ M.read v k
  if Set.null st then return Nothing else do
  let ((i, j), st') = Set.deleteFindMin st
  lift $ M.write v k st'
  insertIv (succ i) 1 (succ j)
  return $ Just i

writeW :: Int -> Int -> S ()
writeW i p = do
  w <- getW
  lift $ M.write w i p

insertIv :: Int -> Int -> Int -> S ()
insertIv i k i_stop
  | i == i_stop = return ()
  | otherwise = do
    v <- getV
    lift $ M.modify v (Set.insert (i, pred j)) k
    insertIv j (succ k) i_stop
  where
  j = i + (2 ^ pred k)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)