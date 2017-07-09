-- 2016-05-14
module Main where

import Data.Bits
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      _ <- getLine
      as <- fmap (map read . words) getLine :: IO [Int]
      -- duurde ff voor ik um doorhad. pas bij het opschrijven van een 5x5 vierkant met P's en N's, en die te vergelijken met die van normale nim game, zag ik het verband (5x5 vierkant is voor twee torens, dimensie 3 zag ik ook voor me, en hogere dimensies gaan hetzelfde)
      putStrLn (if (foldl' xor 0 as /= 0) `xor` (maximum as < 2) then "First" else "Second")
  
    
  