-- 2016-07-24
-- wat we doen is eerst de root zo kiezen dat het gewicht van elke (strikte) subtree maximaal de helft van het totale gewicht is. daardoor zit de root altijd in "de middelste" component van de drie (er zijn twee "zij" components en 1 "middelste" component). hierdoor is het genoeg om twee disjuncte subtrees te vinden van de rooted tree. nu zijn er 2 mogelijkheden: of de twee subtrees hebben even groot gewicht (dan zijn ze sowieso disjunct omdat elke node minstens 1 coin bevat, dus hoeven we daar niet meer op te checken, zie de variabele "try2"), of de middelste component en 1 van de twee subtrees hebben even groot gewicht, en de andere component heeft een kleiner gewicht (dit is "try3", en hierbij moeten we wel uitsluiten dat we naar overlappende subtrees kijken, wat we via de inclusion vector doen). "try1" is het speciale geval waarbij we de boom in precies twee even grote delen knippen.

import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Data.List
import Data.Function
import qualified Data.Map.Strict as Map

type T = (Int, Int, Int)

main :: IO ()
main = do
  rd <- intReader
  [qInput] <- rd 1
  replicateM_ qInput $ do
    [n] <- rd 1
    cv <- fmap V.fromList $ rd n -- coin vector
    esInput <- replicateM (pred n) $ fmap (map pred) $ rd 2
    evm <- M.replicate n [] -- edge vector mutable
    forM_ esInput $ \[u, v] -> do
      M.modify evm (u :) v
      M.modify evm (v :) u
    ev <- V.freeze evm
    wv0 <- getWv n cv ev 0
    let
      m = sum $ V.toList cv -- total number of coins
      root = go 0 (-1)
        where
        go i prev
          | null js = i
          | otherwise = go (head js) i
          where
          js = [j | j <- ev V.! i, j/= prev, (wv0 V.! j) * 2 > m]
    wv <- getWv n cv ev root
    iv <- getIv n ev root -- inclusion vector
    let
      pairs = zip [0..] $ V.toList wv
      rMap = f Map.empty pairs
        where
        f mp [] = mp
        f mp ((i, w) : rest) = Map.insertWith (++) w [p] (f mp rest)
          where
          p = fst $ iv V.! i
      checkOk (i, w) = case (m - 2 * w) `Map.lookup` rMap of
        Nothing -> False
        Just rs -> not $ null $ filter (\r -> r < p || r > q) $ rs
        where
        (p, q) = iv V.! i
      try1 = listToMaybe $ filter (\w -> 2 * w == m) $ V.toList wv
      try2 = listToMaybe $ map head $ filter (\x -> length x > 1) $ group $ sort $ filter (\w -> 3 * w >= m && 2 * w < m) $ V.toList wv
      try3 = listToMaybe $ map snd $ filter checkOk $ sortBy (compare `on` snd) $ filter (\(_, w) -> 3 * w > m && 2 * w < m) $ pairs
      ans = case sort $ map fromJust $ filter isJust [try1, try2, try3] of
        [] -> (-1)
        (w:_) -> 3 * w - m
    print ans
  where
  getWv n cv ev root = do
    v <- M.new n
    forM_ (go root (-1)) $ \(i, c) -> M.write v i c
    V.freeze v
    where
    go i prev = (i, c) : concat xss
      where
      xss = [go j i | j <- ev V.! i, j /= prev]
      c = (cv V.! i) + (sum $ map (snd . head) xss)
  getIv n ev root = do
    v <- M.new n
    forM_ (fst $ go root 0 (-1)) $ \(i, p, q) -> M.write v i (p, q)
    V.freeze v
    where
    go :: Int -> Int -> Int -> ([T], Int)
    go i pI prev = ((i, pI, qI) : restGo, qI)
      where
      (restGo, qI) = f [j | j <- ev V.! i, j /= prev] (succ pI)
      f :: [Int] -> Int -> ([T], Int)
      f [] p = ([], pred p)
      f (j:js) pJ = (xsJ ++ xsRest, qRest)
        where
        (xsJ, qJ) = go j pJ i
        (xsRest, qRest) = f js (succ qJ)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
    
    
    
    