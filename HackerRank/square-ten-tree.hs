-- 2016-05-22
import Data.Char
import Data.Int
import qualified Data.Vector.Unboxed as U

type U8 = U.Vector Int8
type Pair = (Int, String)

main :: IO ()
main = do
  l <- fmap (U.fromList . map (fromIntegral . digitToInt)) getLine
  r <- fmap (U.fromList . map (fromIntegral . digitToInt)) getLine
  sequence_ [putStrLn s | s <- run l r]
  
run :: U8 -> U8 -> [String]
run l r = show (length xs) : [show i ++ " " ++ s | (i, s) <- xs]
  where
    m = head . dropWhile (\x -> 2 ^ (x - 1) < U.length r) $ [1..]
    xs = f l r m
    
f :: U8 -> U8 -> Int -> [Pair]
f l r level
  | m == 0 = [(0, pretty (sub r l (-1)))]
  | l1 == r1 = f l2 r2 m'
  | diff == zero = f l2 ((U.fromList $ 1 : replicate (m - U.length r2) 0) U.++ r2) m'
  | otherwise = p0 ++ p1 ++ p2
  where
    m' = pred level
    m = if level == 0 then 0 else 2 ^ (level - 1)
    n_l = U.length l
    n_r = U.length r
    (l1, l2') = if n_l > m then U.splitAt (n_l - m) l else (zero, l)
    (r1, r2') = if n_r > m then U.splitAt (n_r - m) r else (zero, r)
    l2 = removeZeros l2'
    r2 = removeZeros r2'
    extra = if l2 == one || l2 == zero then 0 else 1
    diff = sub r1 l1 extra
    p0
      | l2 == one = []
      | l2 == zero = [(0, "1")]
      | otherwise = reverse $ f one complement m'
    p2
      | r2 == zero = []
      | otherwise = f one r2 m'
    p1 = [(level, pretty diff)]
    complement = sub (U.fromList $ 1 : replicate m 0) l2 (-1)
    
pretty :: U8 -> String
pretty = map (intToDigit . fromIntegral) . U.toList

removeZeros :: U8 -> U8
removeZeros x
  | U.all (== 0) x = zero
  | U.head x /= 0 = x
  | otherwise = U.dropWhile (== 0) x
  
zero :: U8
zero = U.fromList [0]

one :: U8
one = U.fromList [1]
  
sub :: U8 -> U8 -> Int -> U8
sub r l d = removeZeros $ U.fromList (reverse $ zs1)
  where
    c = if d == 1 then 1 else 0
    zs = g (reverse $ U.toList r) (reverse $ U.toList l) c
    zs1 = if d == -1 then addOne zs else zs
    g [] [] 0 = []
    g [] _ _ = undefined
    g (x:xs) ys c1
      | y1 <= x = (x - y1) : g xs ys' 0
      | otherwise = (10 + x - y1) : g xs ys' 1
      where
        (ys', y1) = if null ys then ([], c1) else (tail ys, head ys + c1)
    addOne [] = [1]
    addOne (9:xs) = 0 : addOne xs
    addOne (x:xs) = (succ x) : xs
