-- 2016-05-07
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Int
import Data.List
import Data.Function (on)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Uv
import qualified Data.Vector.Mutable as Mv
import qualified Data.Vector.Unboxed.Mutable as Umv
import Control.Monad.ST
import Control.Monad.Primitive
import Data.STRef
import Data.Bits

type Id = Int
data Node a = Node Id a [Node a]

---------------------
-- find common root
---------------------

type Weight = Int -- number of nodes in subtree
type Depth = Int
type WNode = Node Weight
type DNode = Node (Depth, Id, Id) -- (depth from root, parent Id, Id of first node in chain)
type DVector = V.Vector DNode

instance Eq (Node a) where
  (Node i _ _) == (Node i1 _ _) = i == i1

getValue :: Node a -> a
getValue (Node _ x _) = x

toWNode :: Node a -> WNode
toWNode (Node i _ ns) = Node i w ns1
  where
    ns1 = fmap toWNode ns
    w = 1 + sum (fmap getValue ns1)

getDNode :: Depth -> Id -> Id -> WNode -> DNode
getDNode d ip iq (Node i _ ns)
  = Node i (d, ip, iq) (map f ns)
  where
    f n@(Node iq1 _ _) = getDNode (d + 1) i (if n == n1 then iq else iq1) n
    n1 = maximumBy (compare `on` getValue) ns

getDVector :: DNode -> Id -> DVector
getDVector n max_id = runST f
  where
    f :: forall s. ST s DVector
    f = do
      v <- Mv.new (max_id + 1) :: ST s (Mv.MVector (PrimState (ST s)) DNode)
      walkNode v n
      xs <- sequence [Mv.read v i | i <- [0..max_id]]
      return (V.fromList xs)
      
getDepth :: Num a => DVector -> Id -> a
getDepth v i = fromIntegral d where (Node _ (d, _, _) _) = v V.! i
      
getParent :: DVector -> Id -> Id
getParent v i = j where (Node _ (_, j, _) _) = v V.! i

walkNode :: PrimMonad m => Mv.MVector (PrimState m) (Node a) -> (Node a) -> m ()
walkNode v n@(Node i _ ns) = do
  Mv.write v i n
  sequence_ [walkNode v n' | n' <- ns]

-- get lowest common ancestor
getCommonRoot :: DVector -> Id -> Id -> Id
getCommonRoot v i j
  | iq == jq = if di < dj then i else j
  | diq < djq = getCommonRoot v i jqp
  | diq == djq = getCommonRoot v iqp jqp
  | otherwise = getCommonRoot v iqp j
  where
    (Node _ (di, _, iq) _) = v V.! i
    (Node _ (dj, _, jq) _) = v V.! j
    (Node _ (diq, iqp, _) _) = v V.! iq
    (Node _ (djq, jqp, _) _) = v V.! jq
    
    
--------------
-- fibonacci  
--------------

type Matrix = (Int64, Int64, Int64, Int64)
    
modulo :: Int64    
modulo = 1000000007

matMult :: Matrix -> Matrix -> Matrix
matMult (e, f, g, h) (a, b, c, d) = ((e * a + f * c) `mod` modulo, (e * b + f * d) `mod` modulo, (g * a + h * c) `mod` modulo, (g * b + h * d) `mod` modulo)

matPow :: Int64 -> [Matrix] -> Matrix
matPow t as = foldl' matMult (1, 0, 0, 1) [b | (b, t1) <- zip as (takeWhile (> 0) (iterate (`quot` 2) t)), t1 `mod` 2 == 1]

powers1 :: [Matrix]    
powers1 = iterate (\a -> matMult a a) (1, 1, 1, 0)

-- negative fibonacci numbers
powers2 :: [Matrix]    
powers2 = iterate (\a -> matMult a a) (0, 1, 1, -1)

-- fibs k = (F_k, F_{k+1})    
fibs :: Int64 -> (Int64, Int64)
fibs t
  | t >= 0 = (x1, x2)
  | otherwise = (y1, y2)
  where
    (x2, x1, _, _) = matPow t powers1
    (y2, y1, _, _) = matPow (-t) powers2
    
fib :: Int64 -> Int64
fib t = let (x, _) = fibs t in x
    
    
--------
-- binary indexed tree
--------
type Pair = (Int, Int)
type BitVector = Uv.Vector Pair -- for each node i, a pair that specifies the start and end indices in the bit array
type Triple = (Int64, Int64, Int64) -- of the form (sum F_{k-1}, sum F_k, sum F_l)
type BitData = Umv.IOVector Triple

getBitVector :: Id -> Node a -> BitVector
getBitVector max_id n = runST f
  where
    f :: forall s. ST s BitVector
    f = do
      v <- Umv.new (max_id + 1) :: ST s (Umv.MVector (PrimState (ST s)) Pair)
      t <- newSTRef 1 :: ST s (STRef s Int)
      g v t n
      xs <- sequence [Umv.read v i | i <- [1..max_id]]
      return (Uv.fromList ((0, 0):xs))
    g v t (Node i _ ns) = do
      t1 <- readSTRef t
      sequence_ [g v t n1 | n1 <- ns]
      t2 <- readSTRef t
      modifySTRef t succ
      Umv.write v i (t1, t2)

handleUpdate :: DVector -> BitVector -> BitData -> Id -> Int64 -> IO ()
handleUpdate v bv bd i k = do
  updateBitData bd t1 (x, y, z)
  updateBitData bd (t2 + 1) (-x, -y, -z)
  where
    d = getDepth v i
    k1 = k - d + 2
    (t1, t2) = bv Uv.! i
    (x, y) = fibs (k1 - 1)
    z = fib (k + 1)
    
updateBitData :: BitData -> Id -> (Int64, Int64, Int64) -> IO ()
updateBitData bd i (dx, dy, dz) = sequence_ [f j | j <- takeWhile (< len) (iterate g i)]
  where
    f = Umv.modify bd (\(x, y, z) -> ((x + dx) `mod` modulo, (y + dy) `mod` modulo, (z + dz) `mod` modulo))
    g j = j + (j .&. (-j))
    len = Umv.length bd
    
handleQuestion :: DVector -> BitVector -> BitData -> Id -> Id -> IO ()
handleQuestion v bv bd i j = do
  t1 <- getSum v bv bd i
  t2 <- getSum v bv bd j
  t3 <- getSum v bv bd ij
  t4 <- getSum v bv bd (getParent v ij)
  print ((t1 + t2 - t3 - t4) `mod` modulo)
  where
    ij = getCommonRoot v i j
    
getSum :: DVector -> BitVector -> BitData -> Id -> IO (Int64)
getSum _ _ _ 0 = return 0
getSum v bv bd i = do
  (t1, t2, t3) <- readBitData bd t
  return ((t2 * y + t1 * x - t3) `mod` modulo)
  where
    (_, t) = bv Uv.! i
    (x, y) = fibs d
    d = getDepth v i
    
readBitData :: BitData -> Id -> IO Triple
readBitData bd i = do
  xs <- sequence [Umv.read bd j | j <- takeWhile (> 0) (iterate g i)]
  return $ foldl' (\(x, y, z) (x1, y1, z1) -> ((x + x1) `mod` modulo, (y + y1) `mod` modulo, (z + z1) `mod` modulo)) (0, 0, 0) xs
  where
    g j = j - (j .&. (-j))
    
---------
-- rest  
---------
  
createGraph :: Id -> [(Id, Id)] -> Node ()
createGraph max_id xs = g (runST f) 1
  where
    f :: forall s. ST s (V.Vector [Id])
    f = do
      v <- Mv.replicate (max_id + 1) [] :: ST s (Mv.MVector (PrimState (ST s)) [Id])
      sequence_ [Mv.modify v (i:) ip | (i, ip) <- xs]
      iss <- sequence [Mv.read v i | i <- [0..max_id]]
      return (V.fromList iss)
    g :: V.Vector [Id] -> Id -> Node ()
    g v i = Node i () [g v j | j <- v V.! i]
    
handleQuery :: DVector -> BitVector -> BitData -> IO ()
handleQuery v bv bd = do
  [x, y, z] <- fmap words getLine
  if x == "Q" then handleQuestion v bv bd (read y) (read z) else handleUpdate v bv bd (read y) (read z)

main :: IO ()    
main = do
  [max_id, query_count] <- fmap (fmap read . words) getLine :: IO [Int]
  n <- fmap (createGraph max_id . zip [2..] . (map read)) (sequence (replicate (max_id - 1) getLine)) :: IO (Node ())
  run max_id query_count n

run :: Id -> Int -> Node () -> IO ()
run max_id query_count n = do
  bd <- Umv.replicate (max_id + 2) (0, 0, 0)
  sequence_ (replicate query_count (handleQuery v bv bd))
  where
    wn = toWNode n
    dn = getDNode 0 0 1 wn
    v = getDVector dn max_id
    bv = getBitVector max_id n
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
--------------
-- debugging
--------------
printNode :: Show a => Node a -> IO ()
printNode (Node i x ns) = do
  putStrLn (show i ++ ": " ++ show x)
  sequence_ [printNode n | n <- ns]
    
printDNode :: DNode -> IO ()
printDNode (Node i (d, ip, iq) ns) = do
  putStrLn (show i ++ ", parent: " ++ show ip ++ ", q:  " ++ show iq ++ ", depth: " ++ show d)
  sequence_ [printDNode n | n <- ns]
  
printBitData :: BitData -> IO ()
printBitData bd = do
  xs <- sequence [Umv.read bd i | i <- [0..Umv.length bd-1]]
  putStrLn $ "bd = " ++ show xs
  
printSums :: DVector -> BitVector -> BitData -> IO ()
printSums v bv bd = do
  xs <- sequence [getSum v bv bd i | i <- [1..Umv.length bd-2]]
  putStrLn $ "sums = " ++ show xs

createInput1 :: IO ()
createInput1 = do
  putStrLn "100000 100000"
  sequence_ [print i | i <- take 99999 is]
  sequence_ [putStrLn $ "U " ++ show (i * 10) ++ " " ++ show (i * i * i) | i <- [1..10000 :: Int64]]
  sequence_ [putStrLn $ "Q " ++ show i ++ " " ++ show j | i <- [1..300 :: Int64], j <- [99701..100000 :: Int64]]
  where
    is = 1:1:2:2:(map (+4) is) :: [Int]

createInput2 :: IO ()
createInput2 = do
  putStrLn "100000 90000"
  sequence_ [print i | i <- take 99999 is]
  sequence_ [putStrLn $ "Q " ++ show i ++ " " ++ show j | i <- [1..300 :: Int64], j <- [99701..100000 :: Int64]]
  where
    is = 1:1:2:2:(map (+4) is) :: [Int]
