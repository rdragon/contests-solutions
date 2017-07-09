-- 2016-07-17
import Control.Monad
import Data.List

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    n <- fmap read getLine
    let ans = if n == 0 then "INSOMNIA" else show (solve n n ['0'..'9'])
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ ans

solve :: Int -> Int -> String -> Int
solve n n0 ds
  | null ds1 = n
  | otherwise = solve (n + n0) n0 ds1
  where
  s = show n
  ds1 = filter (`notElem` s) ds
