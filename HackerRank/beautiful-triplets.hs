-- 2016-05-01
main = do
  d <- fmap (read . (!! 1) . words) getLine
  a <- fmap (fmap read . words) getLine
  print (f d a)

f d a = length . filter id $ zipWith3 (\x y z -> x && y && z) b (drop d b) (drop (d + d) b)
  where
    diffs = zipWith (-) (tail a) a
    b = True : concatMap (\x -> replicate (x - 1) False ++ [True]) diffs