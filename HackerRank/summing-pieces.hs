-- 2016-09-25
-- summing pieces, world codesprint 7
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [n] <- rd 1
  as <- rd n
  print $ type1 as `add` sides as `add` type2 as `add` type3 as

sides :: [Int] -> Int
sides as = let n = length as in side as `add` side (reverse as) `add` (n `mult` foldl' add 0 as)

side :: [Int] -> Int
side as = foldl' add 0 $ zipWith mult xs zs
  where
  n = length as
  xs = scanl1 add $ init as
  ys = iterate (mult two_inv) $ pow 2 (n - 2)
  zs = zipWith mult ys [1..]

type1 :: [Int] -> Int
type1 as
  | n < 3 = 0
  | otherwise = go as1 `add` go (reverse as2)
  where
  n = length as
  (as1, as2) = splitAt (n `quot` 2) as
  go bs = foldl' add 0 vs
    where
    xs = [mult i i | i <- [1..]]
    ys = iterate (mult two_inv) $ pow 2 (n - 3)
    zs = zipWith mult xs ys
    ws = scanl1 add zs
    vs = zipWith mult ws (tail bs)

two_inv :: Int
two_inv = inv 2

type2 :: [Int] -> Int
type2 as = go (tail as1) `add` go (tail $ reverse as2)
  where
  n = length as
  (as1, as2) = splitAt (n `quot` 2) as
  go bs = foldl' add 0 $ zipWith mult bs ws
    where
    xs = zipWith mult [2..n-2] $ iterate (mult two_inv) (pow 2 (n - 4))
    tot = foldl' add 0 xs
    ys = zipWith add xs (reverse xs)
    zs = map (\x -> tot `add` (-x)) $ scanl add 0 ys
    ws = zipWith mult zs [1..]

type3 :: [Int] -> Int
type3 as = go (drop 2 as1) `add` go (drop 2 $ reverse as2)
  where
  n = length as
  (as1, as2) = splitAt (n `quot` 2) as
  go bs = foldl' add 0 $ zipWith mult bs ws
    where
    xs = zipWith mult [n-2, n-3..] [1..]
    ys = zipWith mult xs $ iterate (mult 2) 1
    ws = scanl1 add ys

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)

modulo :: Int
modulo = 1000000007

mult :: Int -> Int -> Int
mult x y = (x * y) `mod` modulo

add :: Int -> Int -> Int
add x y = (x + y) `mod` modulo

inv :: Int -> Int
inv x = pow x (modulo - 2)

pow :: Int -> Int -> Int
pow = go 1
  where
  go x _ 0 = x
  go x a b = go y (mult a a) q
    where
    (q, r) = b `divMod` 2
    y = if r == 0 then x else mult x a