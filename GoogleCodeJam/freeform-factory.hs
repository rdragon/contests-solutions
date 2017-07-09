-- 2016-07-17
-- eerst uitleg moeten lezen

import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Data.List
import Control.Monad

type Q = (Int, Int, Int, Int) -- quadruple
type T = (Int, Int, Int) -- triple

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    n <- fmap read getLine
    ss <- replicateM n getLine
    ans <- solve ss
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans

solve :: [String] -> IO Int
solve ssIn = do
  v <- M.replicate vLen maxBound
  let
    go_m :: [Q] -> [Q] -> Q -> IO ()
    go_m [] _ _ = return ()
    go_m ((wQ, hQ, mQIn, ixQ) : qs) ps (wIn, hIn, mIn, ixIn) = mapM_ go [0 .. min mIn mQIn]
      where
      go :: Int -> IO ()
      go mQ = do
        when (m > 0) $ go_m qs psOut (w, h, m, ix)
        when (m == 0 && (mQ > 0 || ixIn == 0)) $ handleT (psOut ++ qs) (w, h, ix)
        where
        ix = ixIn + ixQ * mQ
        m = mIn - mQ
        w = wIn + mQ * wQ
        h = hIn + mQ * hQ
        psOut = if mQ == mQIn then ps else ((wQ, hQ, mQIn - mQ, ixQ) : ps)

    handleT :: [Q] -> T -> IO ()
    handleT qs (wIn, hIn, ixIn) = forM_ [0 .. pred n] $ \nIn -> do
      x <- M.read v (ixIn + nIn) :: IO Int
      let
        handleQ :: Q -> IO ()
        handleQ (wQ, hQ, _, ixQ) = M.modify v (min (x + nExtra * nExtra)) (ix + nNew)
          where
          w = wIn + wQ
          h = hIn + hQ
          nNew = if w == h then w else nIn
          nExtra = nNew - nIn
          ix = ixIn + ixQ
      when (x < maxBound) $ mapM_ handleQ qs
    
    handle_m :: Int -> IO ()
    handle_m m = go_m qsAll [] (0, 0, m, 0)
  ans2 <- if null qsAll then return 0 else do
    M.write v 0 0
    mapM_ handle_m [0 .. pred mSum] -- forward dynamic programming
    M.read v (ixFinal + nRects)
  return $ ans1 + ans2
  where
  ixFinal = sum $ map (\(_, _, m, ix) -> m * ix) qsAll
  ans1 = (sum $ map ((^ (2 :: Int)) . fst) $ concat squaress) - (sum $ map countOnes ssIn)
  mSum = sum $ map (\(_, _, m, _) -> m) qsAll
  n = length ssIn
  nRects = sum $ map fst $ concat rectss
  rectss :: [[(Int, Int)]]
  rectss
    | n1 == 0 = rectss'
    | otherwise = (replicate n1 (0, 1)) : rectss'
    where
    n1 = sum $ map (\(x, y) -> x - y) $ concat rectss'
  (qsAll, vLen) = foldl' go ([], succ n) rectss
    where
    go :: ([Q], Int) -> [(Int, Int)] -> ([Q], Int)
    go (xs, ix) rects = ((w, h, m, ix) : xs, ix1)
      where
      m = length rects
      ix1 = ix * succ m
      (w, h) = head rects
  squaress, rectss' :: [[(Int, Int)]]
  (squaress, rectss') = partition (uncurry (==) . head) $ group $ sort $ map (fmap countOnes) $ go $ map (\s -> (1, s)) $ ssIn
    where
    go :: [(Int, String)] -> [(Int, String)]
    go a
      | length a == length b = a
      | otherwise = go b
      where
      b = foldl' add [] a
    add :: [(Int, String)] -> (Int, String) -> [(Int, String)]
    add [] x = [x]
    add ((m, t) : xs) x@(m1, s)
      | overlap s t = (m + m1, myUnion s t) : xs
      | otherwise = (m, t) : add xs x
    overlap :: String -> String -> Bool
    overlap xs ys = or $ zipWith f xs ys where
      f '1' '1' = True
      f _ _ = False
    myUnion :: String -> String -> String
    myUnion [] [] = []
    myUnion ('1':xs) (_:ys) = '1' : myUnion xs ys
    myUnion (_:xs) ('1':ys) = '1' : myUnion xs ys
    myUnion (_:xs) (_:ys)   = '0' : myUnion xs ys
  countOnes :: String -> Int
  countOnes = length . filter (== '1')
    