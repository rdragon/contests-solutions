-- 2016-06-06
import Data.List
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Maybe
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad
import Data.Function

type Room = Int
type Score = Int
type Edge = (Room, Score)

main :: IO ()
main = do
  x <- readWords
  n <- readInt x
  triples <- sequence (replicate n (readInts x 3))
  q <- readInt x
  levels <- sequence (replicate q (readInts x 3))
  mapM_ print (run triples levels)
  
run :: [[Int]] -> [[Int]] -> [Int]
run triples levels = map (getScore scores k) levels
  where
    n = length triples
    cds :: V.Vector [Edge] -- corridors
    cds = runST $ do
      v <- M.replicate (succ n) []
      forM_ triples' $ \[a, b, x] -> do
        M.modify v ((b, x) :) a
        M.modify v ((a, -x) :) b
      V.freeze v
    (triples', k') = case getParallelEdge triples of
      Nothing -> (triples, undefined)
      Just (ix, z) -> let (x, (_:y)) = splitAt ix triples in (x ++ y, z)
    scores = runST $ do
      v <- M.new (succ n)
      mapM_ (uncurry $ M.write v) l0
      V.freeze v
    (e0, l0) = walk 0 (Left Set.empty, []) (1, 0)
    k = case e0 of 
      Right (_, x) -> x
      _ -> k'
    walk :: Room -> (Either (Set.Set Room) Edge, [Edge]) -> Edge -> (Either (Set.Set Room) Edge, [Edge])
    walk prev (e, l) (r, m)
      | visited = (e1, l)
      | r == bad = (e, l)
      | otherwise = g $ foldl' (walk r) (e1, (r, m) : l) (map (fmap (+ m)) . filter ((/= prev) . fst) $ cds V.! r)
      where
        (e1, visited, bad) = case e of
          Right (s, _) -> (e, False, s)
          Left set -> if r `Set.member` set
            then (Right (prev, m - fromJust (lookup r l)), True, undefined)
            else (Left (r `Set.insert` set), False, 0)
        g x@(Right _, _) = x
        g (_, l1) = (e, l1)

getParallelEdge :: [[Int]] -> Maybe (Int, Int)
getParallelEdge triples
  | null zs = Nothing
  | otherwise = Just (ix, k)
  where
    ys = map (\[x, y, z] -> if x < y then [x, y, z] else [y, x, -z]) triples
    xs = sortBy (compare `on` init) ys
    zs = filter (\(x, y) -> init x == init y) $ zip xs (tail xs)
    (a, b) = head zs
    k = last b - last a
    Just ix = findIndex (\[x, y, _] -> [min x y, max x y] == init a) triples
    
getScore :: V.Vector Score -> Score -> [Int] -> Int
getScore scores k [r, r1, md] = md - j + i
  where
    m = (scores V.! r1) - (scores V.! r)
    j = gcd k md
    i = m `mod` j
getScore _ _ _ = undefined

readWords :: IO (IORef [B.ByteString])
readWords = fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef

readInt :: IORef [B.ByteString] -> IO Int
readInt ref = do
  x <- readIORef ref
  writeIORef ref (tail x)
  return (fst . fromJust . B.readInt . head $ x)

readInts :: IORef [B.ByteString] -> Int -> IO [Int]
readInts ref n = do
  x <- readIORef ref
  writeIORef ref (drop n x)
  return (take n . map (fst . fromJust . B.readInt) $ x)