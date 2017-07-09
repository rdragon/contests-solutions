-- 2016-06-15
import Data.List
import Control.Monad

data Tree a = Tree (Tree a) a (Tree a)

instance Functor Tree where
  fmap f (Tree l x r) = Tree (fmap f l) (f x) (fmap f r)

forest :: [Tree Int]
forest = map (fmap (uncurry g)) [t b b 1 | b <- [3..]] where
  t b d n = Tree (t b d (n * 2)) (b, d) (t b (d + n) (n * 2))
  g b d = 1 + seek b (pred d) + seek (pred b) (pred d)

seek :: Int -> Int -> Int
seek 1 d = d
seek 2 d = ((succ d) * d) `quot` 2
seek b d
  | b > d = seek d d
  | otherwise = g (forest !! (b - 3)) (d - b)
  where
    g (Tree _ x _) 0 = x
    g (Tree l _ r) n
      | even n = g l (n `quot` 2)
      | otherwise = g r (n `quot` 2)

maxF :: Int
maxF = 4294967296

maxDs :: [Int]
maxDs = 0 : maxF : [head [d | d <- [g b..], seek b d >= maxF] | b <- [2..]] where
  x = fromIntegral $ 2 * maxF :: Double
  g 2 = (truncate . sqrt $ x) - 1
  g b = b

maxB :: Int
maxB = head [b | b <- [1..], seek b b >= maxF]

compute :: Int -> Int -> Int
compute b d
  | b > d = compute d d
  | b >= maxB || d >= (maxDs !! b) = maxF
  | otherwise = seek b d

main :: IO ()
main = do
  m <- fmap read getLine :: IO Int
  forM_ [1..m] $ \i -> do
    [f, d, b] <- fmap (map read . words) getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++
      (intercalate " " . map show $ [fMax b d, minD f b, minB f d])
  where
    fMax b d = if compute b d >= maxF then -1 else compute b d
    minD f 1 = f
    minD f b = head [d | d <- [1..], compute b d >= f]
    minB f d = head [b | b <- [1..], compute b d >= f]
