-- 2016-07-30
{-
is niet meteen duidelijk dat het algoritme snel genoeg is. maar vertices die isolated raken kosten niks (want als er daar geld te halen is dan nemen we het sowieso mee, en anders dan kunnen we zowel langsgaan bij het huis als niet, dus dan doen we later iets maal twee), en vertices met een hoge degree kosten weinig, omdat wanneer we langsgaan bij zo'n vertex er veel vertices uit de graaf wegvallen (alle buren). 

vertices met graad 1 zijn gevaarlijk, want als we de vertices langsgaan zodat we telkens een vertex van graad 1 hebben (bijvoorbeeld wanneer de graaf een horizontaal pad is en we gaan 'm van links naar rechts af), en nergens is geld te halen, dan berekenen we de fibonacci reeks, en als het pad lengte 34 heeft zijn er fib(36) = 14930352 manieren, daar is het algoritme te langzaam voor.

een vertex met graad twee is daarentegen al goed, want dan is de complexiteit te berekenen via f(n) = f(n - 1) + f(n - 3), en dat geeft stuk kleiner getal. dus door een beetje op te letten met welk pad we de graaf doorlopen is brute force met het negeren van isolated vertices snel genoeg. zo'n pad zoeken we in findWalk. (zelfs wanneer we nemen prefix = [] in deze functie, en dus enkel de vertices sorteren op graad, wordt ie al geaccepteerd. echter, met het voorbeeld hierboven van het horizontale pad en nergens geld, is hij dan te langzaam, maar zo'n testcase (of iets dat erop lijkt) is er blijkbaar niet)
-}

import Data.List
import Data.Function
import Control.Monad
import Data.Bits
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

type Dollar = Int
type Vertex = Int
type Mask = Int -- vertex mask

main :: IO ()
main = do
  rd <- intReader
  [n, m] <- rd 2
  ds <- fmap V.fromList $ rd n -- dollars
  es <- M.replicate n (0 :: Mask) -- edges
  let addEdge u v = M.modify es (flip setBit v) u
  input <- replicateM m $ fmap (map pred) $ rd 2
  forM_ input $ \[u, v] -> addEdge u v >> addEdge v u
  es1 <- V.freeze es
  putStrLn $ solve ds es1

solve :: V.Vector Dollar -> V.Vector Mask -> String
solve ds es = let (d, c) = go srt (2 ^ n - 1) 0 0 (0, 0) in show d ++ " " ++ show c
  where
  srt = findWalk n es
  n = V.length ds
  go :: [Vertex] -> Mask -> Int -> Dollar -> (Dollar, Int) -> (Dollar, Int)
  -- pending vertices, pending mask, isolated zero count, accumulating dollars, best pair
  go pendvs pendm zeroc accd bestp
    | null pendvs = updateBest bestp accd (2 ^ zeroc)
    | not $ testBit pendm curv = go nextvs pendm zeroc accd bestp
    | isolated = if curd == 0
      then go nextvs pendm1 (succ zeroc) accd bestp
      else go nextvs pendm1 zeroc (accd + curd) bestp
    | otherwise = let bestp' = go nextvs pendm2 zeroc (accd + curd) bestp in go nextvs pendm1 zeroc accd bestp'
    where
    pendm1 = clearBit pendm curv
    pendm2 = pendm1 .&. (complement cure)
    (curv : nextvs) = pendvs
    curd = dollar curv
    cure = pendm .&. edgeMask curv
    isolated = cure == 0
  edgeMask = (es V.!)
  dollar = (ds V.!)
  updateBest (d, c) d1 c1
    | d1 > d = (d1, c1)
    | d > d1 = (d, c)
    | otherwise = (d, c + c1)

findWalk :: Int -> V.Vector Mask -> [Vertex]
findWalk n es = prefix ++ (srt \\ prefix)
  where
  prefix = go (2 ^ n - 1)
  srt = sortBy (flip compare `on` degree) [0..pred n] where
    degree u = popCount $ (es V.! u)
  go :: Mask -> [Vertex]
  go 0 = []
  go pendvs = v : go pendvs1
    where
    v = maximumBy (compare `on` degree) [u | u <- [0 .. pred n], testBit pendvs u]
    e = es V.! v
    pendvs1 = (clearBit pendvs v) .&. (complement e)
    degree u = popCount $ ((es V.! u) .&. pendvs)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)





-- andere versie, volgens methode uit editorial
import Data.Bits
import Control.Monad
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [n, ec] <- rd 2
  cv <- fmap V.fromList $ rd n
  ev <- do
    v <- M.replicate n (0 :: Int)
    replicateM_ ec $ do
      [i, j] <- fmap (map pred) $ rd 2
      M.modify v (flip setBit i) j
      M.modify v (flip setBit j) i
    V.freeze v
  let
    n' = min 13 n
    traverseIndep :: (Int -> Int -> Int -> IO ()) -> Int -> Int -> IO ()
    traverseIndep f i m = f 0 0 0 >> go 0 i 0 0
      where
      go msk j c nb
        | j == m = return ()
        | testBit nb j = go msk k c nb
        | otherwise = f msk' nb' d >> go msk' k d nb' >> go msk k c nb
        where
        k = succ j
        msk' = setBit msk j
        d = c + (cv V.! j)
        nb' = nb .|. (ev V.! j)
    updateEntry c r (cOld, s)
      | c > cOld  = (c, r)
      | c == cOld = (c, s + r)
      | otherwise = (cOld, s)
  pv <- do
    v <- M.replicate (bit n') ((0, 0) :: (Int, Int))
    let
      update msk c = M.modify v (updateEntry c 1) msk
      handleIndep mskIn _ c = go mskIn 0 >> update mskIn c
        where
        go msk i
          | i == n' = return ()
          | testBit msk i = go msk j
          | otherwise = update msk' c >> go msk j >> go msk' j
          where
          j = succ i
          msk' = setBit msk i
    traverseIndep handleIndep 0 n'
    V.freeze v
  ans <- newIORef (0, 0)
  let
    handleIndep _ nb c = modifyIORef' ans (updateEntry (c + d) r)
      where
      msk = (bit n' - 1) .&. complement nb
      (d, r) = pv V.! msk
  traverseIndep handleIndep n' n
  pair <- readIORef ans
  putStrLn $ show (fst pair) ++ " " ++ show (snd pair)

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
  
