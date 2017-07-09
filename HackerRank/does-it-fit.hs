-- 2016-06-05
main :: IO ()
main = do
  [w, h] <- fmap (map read . words) getLine
  [n] <- fmap (map read . words) getLine
  let
    run = do
      line <- getLine
      putStrLn $ if go line then "YES" else "NO"
    go :: String -> Bool
    go (c:' ':s) = go1 c (map read . words $ s)
    go _ = undefined
    go1 :: Char -> [Int] -> Bool
    go1 'C' [r] = 2 * r <= w && 2 * r <= h
    go1 'R' [w1, h1] = calc (max w h) (min w h) (max w1 h1) (min w1 h1)
    go1 _ _ = undefined
  sequence_ $ replicate n run

calc :: Int -> Int -> Int -> Int -> Bool
calc w h w1 h1
  | w1 <= w && h1 <= h = True
  | h1 > h = False
  | w1 * w1 > w * w + h * h = False
  | otherwise = calc' (fromIntegral w) (fromIntegral h) (fromIntegral w1) (fromIntegral h1)

calc' :: Double -> Double -> Double -> Double -> Bool
calc' w h w1 h1 = or [go a | a <- [0, d .. pi / 4 + d]]
  where
    d = pi / 4 / 100000
    go a = x1 + x <= w && y1 + y <= h
      where
        x = cos a * w1
        y = sin a * w1
        x1 = sin a * h1
        y1 = cos a * h1
  
