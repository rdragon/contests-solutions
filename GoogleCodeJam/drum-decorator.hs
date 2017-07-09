-- 2016-09-01
-- drum decorator, code jam 2015
import Control.Monad
import qualified Data.Vector as V
import Data.List

count :: Int -> Int -> Int
count r c
  | r < 0 = 0
  | r == 0 = 1
  | otherwise = foldl' add 0 [f i j k l | 
    i <- [0 .. r `quot` 3], 
    let r1 = r - 3 * i, 
    j <- [0 .. jMax r1], 
    let r2 = r1 - 4 * j, 
    l <- [0 .. lMax r2],
    let r3 = r2 - 4 * l, 
    r3 `mod` 5 == 0, 
    r3 == 0 || c `mod` 4 == 0, 
    let k = r3 `quot` 5]
  where
  jMax r1 = if c `mod` 6 /= 0 then 0 else r1 `quot` 4
  lMax r1 = if c `mod` 3 /= 0 then 0 else r1 `quot` 4
  f i j k l = fac (i + j + k + l) `mult` (inv $ fac i `mult` fac j `mult` fac k `mult` fac l) `mult` x1
    where
    x1 = g $ concatMap (\(x, y) -> replicate y (1, x)) $ zip [6, 4, 3] [j, k, l]
  g [(x, _)] = x
  g [] = 1
  g ((x, y) : (z, w) : rest) = g $ (x `mult` z `mult` gcd y w, lcm y w) : rest

compute :: Int -> Int -> Int
compute r c = count r c `add` count (r + 2) c `add` count (r - 2) c `add` count r c

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [r, c] <- fmap (map read . words) getLine :: IO [Int]
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show (compute r c)

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

maxFac :: Int
maxFac = 100

facs :: V.Vector Int
facs = V.fromList $ take (succ maxFac) $ map fst $ iterate f (1, 1)
  where
  f (a, b) = (mult a b, succ b)

fac :: Int -> Int
fac = (facs V.!)

{-

test :: IO ()
test = do
  f 1 1 1
  f 2 1 1
  f 2 3 2
  f 2 6 3
  f 3 1 2
  f 3 4 3
  f 4 1 1
  f 4 3 3
  f 4 6 5
  f 5 1 1
  f 5 3 3
  f 5 6 5
  f 5 4 3
  f 5 12 7
  f 6 1 2
  f 6 3 6
  f 6 4 4
  f 6 6 19
  where
  f r c x = if compute r c == x then return () else putStrLn $ "error: " ++ intercalate ", " (map show [r, c, x, compute r c])

-}