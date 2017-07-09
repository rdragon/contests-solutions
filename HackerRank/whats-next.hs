-- 2016-05-03
import Data.List


getC' [a0] = [1, 1, a0 - 1]
getC' [a0, a1] = [1, a1 + 1, a0 - 1]

getC' aa
-- ... 0000   1111
-- ... 000 1 0 111
  | n `mod` 2 == 1 && a2 > 1 = min2 ++ [a2 - 1, 1, 1, a1 - 1]
  
-- ... 1111 0 1111
-- ... 11111 0 111
  | n `mod` 2 == 1 && a2 == 1 = min3 ++ [a3 + 1, 1, a1 - 1]
  
-- ... 0000 1111  0000
-- ... 000 1 00000 111  
  | n `mod` 2 == 0 && a3 > 1 = min3 ++ [a3 - 1, 1, a1 + 1, a2 - 1]
  
-- ... 1111 0 1111 0000
-- ... 11111 00000  111
  | n `mod` 2 == 0 && a3 == 1 = min4 ++ [a4 + 1, a1 + 1, a2 - 1]
  
  where
    n = length aa
    a1 = head $ drop 0 $ reverse aa
    a2 = head $ drop 1 $ reverse aa
    a3 = head $ drop 2 $ reverse aa
    a4 = head $ drop 3 $ reverse aa
    min2 = reverse $ drop 2 $ reverse aa
    min3 = reverse $ drop 3 $ reverse aa
    min4 = reverse $ drop 4 $ reverse aa

main = do
  [t] <- fmap (fmap read . words) getLine
  sequence_ [runTestCase | i <- [0..t-1]]
  where
    runTestCase = do
      getLine
      aa <- fmap (fmap read . words) getLine :: IO [Integer]
      let
        c' = getC' aa
        c = if last c' == 0 then init c' else c'
      printC c
      
printC c = do
  putStrLn (show (length c))
  putStrLn (intercalate " " (fmap show c))
  
