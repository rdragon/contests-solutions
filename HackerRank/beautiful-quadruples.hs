-- 2016-05-22
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed as V
import Data.Bits
import Data.List
import Control.Monad.ST

main :: IO ()
main = do
  input <- fmap (sort . map read . words) getLine
  print (run input)
  
run :: [Int] -> Int
run [aa, bb, cc, dd] = tot - bla
  where
    bla = sum [lookup1 V.! ((a `xor` b) * 3001 + b) | a <- [1..aa], b <- [a..bb]]
    tot = sum [lookup1 V.! (4096 * 3001 + b) | a <- [1..aa], b <- [a..bb]]
    lookup1 = runST $ do
      v <- M.replicate (4097 * 3001) 0
      sequence_ [M.modify v succ (4096 * 3001 + c) | c <- [1..cc], _ <- [c..dd]]
      sequence_ [M.modify v succ ((c `xor` d) * 3001 + c) | c <- [1..cc], d <- [c..dd]]
      let
        f i = do
          x <- M.read v (succ i)
          M.modify v (+x) i
      sequence_ [f (i * 3001 + j) | i <- [0..4096], j <- [2999, 2998..1]]
      V.freeze v
run _ = undefined