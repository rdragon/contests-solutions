-- 2016-05-13
module Main where

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      n <- fmap read getLine :: IO Int
      putStrLn (if a !! (n - 1) then "First" else "Second")
  
a :: [Bool]
a = False:True:True:True:True:[not (x && y && z) | (x, y, z) <- zip3 a (drop 2 a) (drop 3 a)]
    
  