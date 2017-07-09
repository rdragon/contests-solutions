-- 2016-05-17
module Main where

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  sequence_ (replicate t run)
  where
    run = do
      n <- fmap read getLine :: IO Int
      print (calc n)
      
calc :: Int -> Int
calc n = (\(_, b, _) -> b) . last $ takeWhile (\(a, _, _) -> a <= n) xs

xs :: [(Int, Int, Int)]
xs = (2, 4, 1) : f (3, 5, [xs], [i * i + 4 | i <- [2..]], [i * i | i <- [3..]])
  where
    f (a, b, xss, (y:ys), (z:zs)) = (a, b, l) : f (a + l, b + 1, xss2, ys1, zs1)
      where
        xss1 = map tail xss
        (l0, xss2, ys1) = if y == b then (1, tail xs : xss1, ys) else (0, xss1, y:ys)
        (l1, zs1) = if z == b then (1, zs) else (0, z:zs)
        l = l0 + l1 + (sum . map ((\(_, _, c) -> c) . head) $ xss)
