-- 2016-08-27
import Control.Monad
import Data.Char

main :: IO ()
main = do
  q <- fmap read getLine :: IO Int
  replicateM_ q $ do
    a <- getLine
    b <- getLine
    putStrLn $ if f a b then "YES" else "NO"
  where
  f [] [] = True
  f (c:cs) [] = isLower c && f cs []
  f [] _ = False
  f (a:as) (b:bs) = (toUpper a == b && f as bs) || (isLower a && f as (b:bs))
    

