-- 2016-04-17
d = 365
q = d / (d + 1)

pr :: Int -> Double
pr n = 1 - product [(d - fromIntegral i) / d | i <- [0 .. n - 1]]

pr' :: Int -> Double
pr' n 
  = if k == 0 then pr n else q0 * pr n + q1 * pr (n - 1) + q2plus
  where
    q0 = q^k
    q1 = (fromIntegral k) / 366 * q^(k - 1)
    q2plus = 1 - q0 - q1
    k = n `quot` 4
main = do
  t <- fmap read getLine
  sequence_ $ replicate t $ do
    p <- fmap read getLine
    print (g p)

g :: Double -> Int
g 1 = 367
g p = head [n | n <- [2..367], pr' n >= p]