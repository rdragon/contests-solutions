-- 2016-10-20
-- academy-surveillance, week of code 24

import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as B
import Data.List (tails, foldl')

main :: IO ()
main = mapM_ (print. solve. map (fst. fromJust. B.readInt)). chunk 2. tail. B.words =<< B.getContents

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (a, b) = splitAt n xs in a : chunk n b

solve :: [Int] -> Int
solve [w, h] = x1
  where
  hs = map (`quot` 3) [h .. h + 2]
  ws = map (`quot` 3) [w .. w + 2]
  x1 =
    foldl' add 0 [pow 3 (x + y) | (x, y) <- f hs ++ f ws]
      `add`
    foldl' add 0 [pow 3 x | x <- hs ++ ws]
      `sub`
    36
      `add`
    foldl' add 0 [((pow 2 y - 2) `mult` (pow 3 x - 3)) `add` ((pow 3 y `sub` pow 2 y `sub` 1) `mult` (pow 2 x - 2)) | x <- hs, y <- ws]
  f xs = [(x, y) | (x:ys) <- tails xs, y <- ys]

modulo :: Int
modulo = 1000000007

mult :: Int -> Int -> Int
mult x y = (x * y) `mod` modulo

add :: Int -> Int -> Int
add x y = (x + y) `mod` modulo

sub :: Int -> Int -> Int
sub x y = (x - y) `mod` modulo

pow :: Int -> Int -> Int
pow = go 1
  where
  go x _ 0 = x
  go x a b = go y (mult a a) q
    where
    (q, r) = b `divMod` 2
    y = if r == 0 then x else mult x a
