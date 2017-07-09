-- 2016-04-17
main = do
  q <- fmap read getLine
  sequence_ $ replicate q $ do
    (l, r) <- readPair
    print (v l r)
  where
    readPair = do
      (s1,  ' ' : s2) <- fmap (span (/= ' ')) getLine
      return (read s1, read s2)

s n = (1044 * i1296 * s4 n - 648 * i216 * s3 n + 164 * n) `mod` m
v 1 r = s r
v l r = (s r - s (l - 1)) `mod` m

m=10^9+7
s3 n = (i4 * (pow m n 4 + 2 * pow m n 3 + pow m n 2)) `mod` m
s4 n = (i30 * (6 * pow m n 5 + 15 * pow m n 4 + 10 * pow m n 3 - n)) `mod` m

i4 = inv m 4
i30 = inv m 30
i1296 = inv m (6^4)
i216 = inv m (6^3)

inv :: Integer -> Integer -> Integer
inv p a = y
  where
    (_, y) = f p a
    f :: Integer -> Integer -> (Integer, Integer)
    f _ 1 = (0, 1)
    f a b =
      let (n, m') = a `quotRem` b
          (xb, xm) = f b m'
      in (xm, (xb - n * xm) `mod` p)
      
pow :: Integer -> Integer -> Int -> Integer
pow p r a = f 1 r a
  where f :: Integer -> Integer -> Int -> Integer
        f x _ 0 = x
        f x r a = f x' ((r * r) `mod` p) a'
          where x' = if bit == 1 then ((x * r) `mod` p) else x
                (a', bit) = a `quotRem` 2