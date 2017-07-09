-- 2016-06-29
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

data S = S {getP :: Int, getE :: Int, getD :: Int, getIx :: Int}

main :: IO ()
main = do
  f <- intReader
  [n, m, k] <- f 3
  ss <- fmap (map (\(ix, [p, c]) -> S (p * 100) (p * c) (p * (100 - c)) ix) . zip [0..]) . sequence . replicate n . f $ 2
  print (solve ss k (m - k))

solve :: [S] -> Int -> Int -> Int
solve ss 0 l = sum . map getE . take l . sortBy (flip compare `on` getE) $ ss
solve ss k l = m + solve' Set.empty mpD ssE ssP l
  where
    (ss1, ss2) = splitAt k . sortBy (flip compare `on` getP) $ ss
    m = sum . map getP $ ss1
    mpD = foldl' (flip inc) Map.empty . map getD $ ss1
    ssE = sortBy (flip compare `on` getE) ss2
    ssP = sortBy (flip compare `on` getP) ss2

solve' :: Set.Set Int -> Map.Map Int Int -> [S] -> [S] -> Int -> Int
solve' _ _ _ _ 0 = 0
solve' set mpD ssE ssP l = m + solve' set' mpD' ssE' ssP' (pred l)
  where
    sP = head ssP
    sE = head ssE
    d = fst . Map.findMin $ mpD
    mE = getE sE
    mP = getP sP - d
    (m, ix, mpD') = if mP > mE then (mP, getIx sP, inc (getD sP) . dec d $ mpD) else (mE, getIx sE, mpD)
    set' = Set.insert ix set
    ssE' = dropWhile ((`Set.member` set') . getIx) ssE
    ssP' = dropWhile ((`Set.member` set') . getIx) ssP

inc :: Int -> Map.Map Int Int -> Map.Map Int Int
inc = Map.alter (maybe (Just 1) $ (Just . succ))

dec :: Int -> Map.Map Int Int -> Map.Map Int Int
dec = Map.update (\x -> if x == 1 then Nothing else Just (pred x))
  
intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)