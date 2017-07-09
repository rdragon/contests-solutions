-- 2016-12-19
-- prime-digit-sums, world codesprint 8
-- solution is in .c file
import qualified Data.Vector.Unboxed as V
import Data.Char
import Data.List

type Four = String

main :: IO ()
main = ans
  where
  fivesEnd = nub $ map tail (make 6)
  fivesStart = nub $ filter (\x -> head x /= '0') $ map init (make 6)
  foursEnd = nub $ map (drop 2) (make 6)
  foursStart = nub $ filter (\x -> head x /= '0') $ map (init. init) (make 6)
  guys = filter inLoop foursEnd
  endings = concatMap extend guys
  endCounts = map (length. extend' 4) guys
  mat = [if canReach x y then 1 else 0 | y <- guys, x <- guys]
  startCounts = [length $ filter (f w) (brute 8) | w <- guys]
    where f w x = drop 4 x == w
  
  ans = print [length $ brute x | x <- take 5 [13..]]
  
  brute :: Int -> [String]
  brute = \n -> filter (\s -> head s /= '0') $ map reverse $ go "" n
    where
    go s 0 = [s]
    go s n = concatMap (\c -> go (c:s) (n - 1)) [c | c <- ['0'..'9'], ok (take 5 (c:s))]
  
  extend :: Four -> [String]
  extend s = s : (map (head s :) $ concatMap extend xs)
    where
    xs = [tail x | x <- fivesEnd, init x == s, tail x `notElem` guys]
    
  extend' :: Int -> Four -> [String]
  extend' 0 s = [s]
  extend' n s = map (head s :) $ concatMap (extend' (n - 1)) xs
    where
    xs = [tail x | x <- fivesEnd, init x == s]
  
  inLoop :: Four -> Bool
  inLoop s = go [s] foursEnd
    where
    go [] _ = False
    go (x:xs) ys
      | s `elem` as = True
      | otherwise = go (as ++ xs) bs
      where
      (as, bs) = partition (canReach x) ys
      
  canReach :: Four -> Four -> Bool
  canReach x y = any (\z -> tail z == y && init z == x) fivesEnd
  
  make :: Int -> [String]
  make n = filter ok [s | i <- [(0 :: Int)..10^n - 1], let s = take n $ (reverse $ show i) ++ repeat '0']
    
  ok :: String -> Bool
  ok s = b 3 && b 4 && b 5
    where
    xs = map digitToInt s
    b k | n < k = True
    b k = go (sum (take k xs)) (drop k xs) xs
    n = length xs
    go p _ _ | not (is_prime p) = False
    go _ [] _ = True
    go p (x:xs) (y:ys) = go (p + x - y) xs ys
    
  is_prime = (bools V.!)
  bools = V.fromList [i `elem` ps | i <- [0..45]]
  ps = take 100 primes
  
primes :: [Int]
primes = f [2..] where f (p:xs) = p : f [x | x <- xs, x `mod` p /= 0]