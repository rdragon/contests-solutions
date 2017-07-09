-- 2016-06-05
import Data.List

type Order = [Int]
type Stock = [Int]

main :: IO ()
main = do
  s <- fmap (map read . words) getLine -- stock
  [n] <- fmap (map read . words) getLine :: IO [Int]
  os <- sequence $ replicate n getOrder -- orders
  print (run s os)
  where
    getOrder :: IO Order
    getOrder = fmap (\line -> map ((\b -> if b then 1 else 0) . (`elem` line)) ['A'..'C']) getLine

run :: Stock -> [Order] -> Int
run s os = count1 + count2 + count3
  where
    (os23, count1, s1) = g1 s os
    (os2, os3) = partition ((== 2) . sum) os23
    (count2, s2) = g2 os2 s1
    count3 = minimum (length os3 : s2)

g1 :: Stock -> [Order] -> ([Order], Int, Stock)
g1 s os = (os23, sum d, s1)
  where
    (os1, os23) = partition ((== 1) . sum) os
    t = foldl' (zipWith (+)) [0, 0, 0] os1
    d = zipWith min t s
    s1 = zipWith (-) s d

g2 :: [Order] -> Stock -> (Int, Stock)
g2 os2 s = f s a1 b1 c1 0
  where
    a1 = length . filter (== 0) . map (!! 0) $ os2
    b1 = length . filter (== 0) . map (!! 1) $ os2
    c1 = length . filter (== 0) . map (!! 2) $ os2
    f [x, y, z] a b c n
      | x == x1 && a > 0 && y > 0 && z > 0 = f [x, pred y, pred z] (pred a) b c (succ n)
      | y == x1 && b > 0 && x > 0 && z > 0 = f [pred x, y, pred z] a (pred b) c (succ n)
      | z == x1 && c > 0 && x > 0 && y > 0 = f [pred x, pred y, z] a b (pred c) (succ n)
      | x == y1 && a > 0 && y > 0 && z > 0 = f [x, pred y, pred z] (pred a) b c (succ n)
      | y == y1 && b > 0 && x > 0 && z > 0 = f [pred x, y, pred z] a (pred b) c (succ n)
      | z == y1 && c > 0 && x > 0 && y > 0 = f [pred x, pred y, z] a b (pred c) (succ n)
      |            a > 0 && y > 0 && z > 0 = f [x, pred y, pred z] (pred a) b c (succ n)
      |            b > 0 && x > 0 && z > 0 = f [pred x, y, pred z] a (pred b) c (succ n)
      |            c > 0 && x > 0 && y > 0 = f [pred x, pred y, z] a b (pred c) (succ n)
      | otherwise = (n, [x, y, z])
      where
        [x1, y1, _] = sort [x, y, z]
    f _ _ _ _ _ = undefined