-- 2016-06-27
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Sequence as Seq
import Data.List
import Control.Monad
import Data.Function
import Control.Exception
import Debug.Trace
import qualified Data.Foldable as Fold
import qualified Data.Vector as V
import qualified Data.Set as Set
import qualified Data.Vector.Mutable as M

type City = Int
type Power = Int
data Road = Road City City Power deriving (Show, Ord, Eq)
data Edge = Edge City Power deriving (Show, Ord, Eq)

main :: IO ()
main = do
  rd <- intReader
  [n, m] <- rd 2
  input <- sequence . replicate m $ fmap (\[a, b, c] -> Road a b c) $ rd 3
  s <- solve n input
  putStrLn s

solve :: Int -> [Road] -> IO String
solve n input = do
  temp1 <- getTree n input
  let
    m = length input
    edges' = mkTree n temp1
    edges = V.fromList (Fold.toList edges')
    (_, ws') = computeWeight (Seq.replicate (succ n) 0) edges 1 Nothing
    ws = V.fromList (Fold.toList ws')
    temp = compute n (Seq.replicate m 0) edges ws 1 Nothing
  return (pretty (Fold.toList temp))

roadP (Road _ _ p) = p
edgeC (Edge x _) = x

pretty zs = reverse $ go $ reverse $ dropWhile (== 0) $ reverse zs
  where
  go [] = []
  go (x:xs)
    | odd x = '1' : rest
    | otherwise = '0' : rest
    where
    rest = go ys
    ys = case xs of
      [] -> if y == 0 then [] else [y]
      (x1:xs1) -> (x1 + y) : xs1
    y = x `quot` 2

computeWeight :: Seq.Seq Int -> V.Vector [Edge] -> Int -> Maybe Int -> (Int, Seq.Seq Int)
computeWeight ws0 edges a b_mb = (w1, ws2)
  where
  (w1, ws1) = foldl' g (1, ws0) nbs where
    g (w, ws) b = let (w', ws') = computeWeight ws edges b (Just a) in (w' + w, ws')
    nbs = filter f $ map edgeC $ edges V.! a where
      f = case b_mb of
        Nothing -> const True
        Just b -> (/= b)
  ws2 = Seq.update a w1 ws1

compute :: Int -> Seq.Seq Int -> V.Vector [Edge] -> V.Vector Int -> Int -> Maybe (Int, Int) -> Seq.Seq Int
compute n sums0 edges ws a mb = sums2
  where
  wA = ws V.! a
  (sums1, bad) = case mb of
    Nothing -> (sums0, 0)
    Just (b, p) -> (Seq.update p (wA * (n - wA)) sums0, b)
  sums2 = foldl' (\sums (Edge b p) -> if b == bad then sums else compute n sums edges ws b (Just (a, p))) sums1 (edges V.! a)

mkTree :: Int -> [Road] -> Seq.Seq [Edge]
mkTree n rs0 = go (Seq.replicate (succ n) []) rs0
  where
  go :: Seq.Seq [Edge] -> [Road] -> Seq.Seq [Edge]
  go s [] = s
  go s (Road a b p : rest) = go (addNb a b p . addNb b a p $ s) rest
  addNb :: City -> City -> Power -> Seq.Seq [Edge] -> Seq.Seq [Edge]
  addNb x b p s = Seq.adjust (Edge b p :) x s

getTree :: Int -> [Road] -> IO [Road]
getTree n input = do
  v <- V.thaw (V.generate (succ n) id)
  w <- V.thaw (V.generate (succ n) (\i -> (1 :: Int, [i])))
  let
    go :: [Road] -> IO [Road]
    go [] = return []
    go (t@(Road a b _) : rest) = do
      ca <- M.read v a
      cb <- M.read v b
      if ca == cb then go rest else do
        (na, as) <- M.read w ca
        (nb, bs) <- M.read w cb
        if na > nb then do
          mapM_ (flip (M.write v) ca) bs
          M.write w ca (na + nb, bs ++ as)
        else do
          mapM_ (flip (M.write v) cb) as
          M.write w cb (na + nb, as ++ bs)
        rest' <- go rest
        return (t : rest')
  go (sortBy (compare `on` roadP) input)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
