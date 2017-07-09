-- 2016-05-22

main :: IO ()
main = do
  [_, k] <- fmap (map read . words) getLine
  st <- getLine
  let n = length st
  let s = take (n `quot` 2) st
  let t = take (n `quot` 2) (reverse st)
  let (u, a) = run s t k
  let d = if odd n then [if a > 0 then '9' else st !! (n `quot` 2)] else []
  let v = u ++ d ++ reverse u
  putStrLn (if null u && n > 1 then "-1" else v)

run :: String -> String -> Int -> (String, Int)
run s t k
  | k < m = ([], 0)
  | otherwise = f (k - m) s t
  where
    m = countErrors s t
    countErrors [] _ = 0
    countErrors _ [] = 0
    countErrors (d:s1) (e:t1) = (if d /= e then 1 else 0) + countErrors s1 t1

f :: Int -> String -> String -> (String, Int)
f a [] _ = ([], a)
f a _ [] = ([], a)
f a ('9':s) (_:t) = let (s1, a1) = f a s t in ('9':s1, a1)
f a (_:s) ('9':t) = let (s1, a1) = f a s t in ('9':s1, a1)
f a (d:s) (e:t)
  | d == e && a >= 2 = let (s1, a1) = f (a - 2) s t in ('9':s1, a1) 
  | d /= e && a >= 1 = let (s1, a1) = f (a - 1) s t in ('9':s1, a1) 
  | otherwise = let (s1, a1) = f a s t in ((max d e):s1, a1)
