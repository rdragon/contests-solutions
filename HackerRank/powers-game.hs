-- 2016-05-16
module Main where

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      n <- fmap read getLine :: IO Int
      putStrLn $ if n `mod` 8 == 0 then "Second" else "First"
      