-- 2016-06-27
import Data.List
import Data.Bits
import Data.Char
import Debug.Trace

type A = Bool
type B = Bool
type C = Bool
type K = Int

main :: IO ()
main = do
  q <- fmap read getLine
  sequence_ . replicate q $ do
    k <- fmap read getLine
    xs <- sequence . replicate 3 $ getLine
    putStrLn (intercalate "\n" (solve xs k))

solve :: [String] -> Int -> [String]
solve ss k
    | k1 > k = ["-1"]
    | otherwise = showTrs trs2
  where
  trs = getTrs ss
  (trs1, k1) = stage1 trs
  trs2 = stage2 trs1 (k - k1)

getTrs :: [String] -> [(A, B, C)]
getTrs ss = trs
  where
  ls = map length ss
  n = maximum ls
  as = map (concatMap rd) ss
  trs = let [as', bs', cs'] = zipWith (\xs l -> replicate (4 * (n - l)) False ++ xs) as ls in zip3 as' bs' cs'

showTrs :: [(A, B, C)] -> [String]
showTrs trs = [go as, go bs]
  where
  (as, bs, _) = unzip3 trs
  go [] = "0"
  go (False : False : False : False : xs) = go xs
  go xs = pretty xs

pretty :: [Bool] -> String
pretty [] = []
pretty (False : False : False : False : xs) = '0' : pretty xs
pretty (False : False : False : True   : xs) = '1' : pretty xs
pretty (False : False : True : False   : xs) = '2' : pretty xs
pretty (False : False : True : True    : xs) = '3' : pretty xs
pretty (False : True : False : False   : xs) = '4' : pretty xs
pretty (False : True : False : True   : xs) = '5' : pretty xs
pretty (False : True : True : False   : xs) = '6' : pretty xs
pretty (False : True : True : True   : xs) = '7' : pretty xs
pretty (True : False : False : False : xs) = '8' : pretty xs
pretty (True : False : False : True  : xs) = '9' : pretty xs
pretty (True : False : True : False  : xs) = 'A' : pretty xs
pretty (True : False : True : True   : xs) = 'B' : pretty xs
pretty (True : True : False : False  : xs) = 'C' : pretty xs
pretty (True : True : False : True   : xs) = 'D' : pretty xs
pretty (True : True : True : False   : xs) = 'E' : pretty xs
pretty (True : True : True : True    : xs) = 'F' : pretty xs
    

rd :: Char -> [Bool]
rd c = map (testBit n) [3, 2, 1, 0]
  where
    n
      | c >= '0' && c <= '9' = digitToInt c
      | otherwise = ord c - ord 'A' + 10

stage1 :: [(A, B, C)] -> ([(A, B, C)], K)
stage1 trs = (map fst ps, sum (map snd ps))
  where
  ps = map ht trs

stage2 :: [(A, B, C)] -> K -> [(A, B, C)]
stage2 trs 0 = trs
stage2 [] _ = []
stage2 ((True, False, True) : xs) k | k >= 2 = (False, True, True) : stage2 xs (k - 2)
stage2 ((True, True, True) : xs) k | k >= 1 = (False, True, True) : stage2 xs (k - 1)
stage2 (x:xs) k = x : stage2 xs k

ht :: (A, B, C) -> ((A, B, C), K)
ht (False, False, False) = ((False, False, False), 0)
ht (False, True, False) = ((False, False, False), 1)
ht (True, False, False) = ((False, False, False), 1)
ht (True, True, False) = ((False, False, False), 2)
ht (False, False, True) = ((False, True, True), 1)
ht (False, True, True) = ((False, True, True), 0)
ht (True, False, True) = ((True, False, True), 0)
ht (True, True, True) = ((True, True, True), 0)
