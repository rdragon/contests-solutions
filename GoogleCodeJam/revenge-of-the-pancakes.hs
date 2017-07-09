-- 2016-07-11
-- we reduceren elke stack tot -+-+... of +-+-...

{-# LANGUAGE ViewPatterns #-}

import qualified Data.Vector as V
import Control.Monad
import Data.List

main :: IO ()
main = do
  test_c <- fmap read getLine :: IO Int
  forM_ [1..test_c] $ \test_n -> do
    s <- getLine
    let ans = compute s
    putStrLn $ "Case #" ++ show test_n ++ ": " ++ show ans
  where
  v = V.generate 202 gen
  gen :: Int -> Int
  gen ix = case ix `divMod` 2 of
    (1, 0) -> 1
    (1, 1) -> 0
    (n, h) -> 1 + (minimum $ map compute $ map (maneuver (getStack n h)) [1, 3 .. pred n])
  getStack n h = take n $ drop h (cycle "-+")
  maneuver s n = let (a, b) = splitAt n s in (map swap $ reverse a) ++ b
  swap '-' = '+'
  swap '+' = '-'
  compute s = let t = map head $ group s in v V.! (2 * length t + (if head t == '+' then 1 else 0))
