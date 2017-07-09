-- 2016-05-01
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Vector.Unboxed as Vec
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.ST

data Graph = Graph Int [(Int, Int)]

emptyGraph :: Graph
emptyGraph = Graph 0 []

printGraph :: Graph -> IO ()
printGraph (Graph n edges) = do
  putStrLn (show n ++ " " ++ show (length edges))
  forM_ edges $ \(u, v) -> putStrLn (show (u + 1) ++ " " ++ show (v + 1))

main = do
  [p, q] <- fmap (fmap read . words) getLine :: IO [Int]
  printGraph (if q == 2 then create2 p else create q p)

-- q = 2
create2 p = f p 100 emptyGraph
  where
    f 0 _ graph = graph
    f p n graph
      | delta_n <= n = f (p - delta_p) (n - delta_n) (add2 graph m)
      | otherwise = undefined
      where
        m = pred $ head (dropWhile (\x -> choose x 3 <= p) [3..])
        delta_p = choose m 3
        delta_n = m + 1
        
-- add component for q = 2        
add2 :: Graph -> Int -> Graph
add2 (Graph n edges) m = Graph n' edges'
  where
    n' = n + m + 1
    edges' = [(n+i, n) | i <- [1..m]] ++ edges

-- assumption: 0 <= k <= n
choose :: Int -> Int -> Int
choose n k = product [n, n-1 .. n-k+1] `quot` product [1..k]

create :: Int -> Int -> Graph
create q p
   = fromJust (f p 100 emptyGraph)
  where
    extra
      | q `mod` 2 == 0 = ((q - 2) `quot` 2) * 3 + 1
      | otherwise = ((q - 3) `quot` 2) * 3 + 3
    f :: Int -> Int -> Graph -> Maybe Graph
    f p n graph
      | n < extra + 3 = Nothing
      | not (null direct_triples) = Just (make_graph (head direct_triples))
      | not (null graphs) = Just (head graphs)
      | otherwise = Nothing
      where
        direct_triples = [(a, b, c) | a <- [1..n-2-extra], b <- [1..n-a-1-extra], c <- [1..n-a-b-extra], a*b*c==p]
        m = pred $ head (dropWhile (\x -> x*x*x < p) [1..])
        k = 3
        triples = [(a, b, c) | a <- [m, m-1 .. 1], b <- [m+k, m+k-1 .. a], c <- [m+k, m+k-1 .. b]
          , a*b*c < p && p `quot` 2 < a*b*c]
        graphs = map fromJust $ filter isJust [f (p - a*b*c) (n - extra - (a+b+c)) (make_graph triple) | triple@(a, b, c) <- triples]
        make_graph triple = ((if q `mod` 2 == 0 then add0 else add1) q graph triple)
        
-- q even
add0 :: Int -> Graph -> (Int, Int, Int) -> Graph
add0 q (Graph n edges) (a, b, c) = g3
  where
    g0 = Graph (n + 1) edges
    g1 = addFlower g0 n l a
    g2 = addFlower g1 n l b
    g3 = addFlower g2 n l c
    l = (q - 2) `quot` 2
    
-- q odd
add1 :: Int -> Graph -> (Int, Int, Int) -> Graph
add1 q (Graph n edges) (a, b, c) = g3
  where
    g0 = Graph (n + 3) ([(n, n+1), (n+1, n+2), (n+2, n)] ++ edges)
    g1 = addFlower g0 n l a
    g2 = addFlower g1 (n+1) l b
    g3 = addFlower g2 (n+2) l c
    l = (q - 3) `quot` 2

addFlower :: Graph -> Int -> Int -> Int -> Graph
addFlower (Graph n e0) m l a = Graph (n+l+a) e2
  where
    path = m:[n..n+l-1]
    e2 = zip path (tail path) ++ e1
    e1 = [(last path, n+l+i) | i <- [0..a-1]] ++ e0

    
    
    
    
-- TESTING

main2 = do
  forM_ [(p, q) | p <- [1..1000], q <- [2..9]] $ \(p, q) ->
    let graph = if q == 2 then create2 p else create q p in check graph p q

check :: Graph -> Int -> Int -> IO ()
check graph@(Graph n edges) p q =
  when (p /= actual_p) $ do
    printGraph graph
    print ("p, q, actual_p: " ++ intercalate ", " (fmap show [p, q, actual_p]))
    print (show distances)
    guard False -- hack to stop the program
  where
    actual_p = length [(x, y, z) | x <- [0..n-1], y <- [x+1..n-1], z <- [y+1..n-1], dist x y == q && dist x z == q && dist y z == q]
    distances = getDistances graph
    dist x y = distances Vec.! (getIndex n x y)
    
getIndex :: Int -> Int -> Int -> Int
getIndex n x y = x + (n * y)

getDistances :: Graph -> Vec.Vector Int
getDistances graph@(Graph n edges) = runST $ do
  distances <- MVec.replicate (n * n) (0 :: Int)
  sequence_ [walkFrom graph distances x | x <- [0..n-1]]
  values <- sequence [MVec.read distances x | x <- [0..n*n-1]]
  return (Vec.fromList values)

walkFrom :: forall s. Graph -> MVec.MVector s Int -> Int -> ST s ()
walkFrom graph@(Graph n _) distances x = f [x] [] 0 where
  -- f ys zs m
  --   ys: nodes left that are reachable in m moves
  --   zs: nodes yet to try, reachable in m+1 moves
  f :: [Int] -> [Int] -> Int -> ST s ()
  f [] [] _ = return ()
  f [] ys m = f ys [] (m + 1)
  f (y:ys) zs m = do
    zs' <- forM (getNbs graph y) g
    f ys (concat zs' ++ zs) m
    where
      g :: Int -> ST s [Int]
      g z = do
        dist <- MVec.read distances i
        if dist == 0
          then do
            MVec.write distances i (m + 1)
            return [z]
          else
            return []
        where
          i = (getIndex n x z)

getNbs :: Graph -> Int -> [Int]
getNbs (Graph _ edges) x = [if a == x then b else a | (a, b) <- edges, a == x || b == x]