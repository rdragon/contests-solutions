import Data.List

main = interact $ show . length . filter f . group . sort . map walk . lines
    where f = (== 1) . (`mod` 2) . length

walk :: String -> (Int, Int)
walk s = foldl1' f . tail . map snd . takeWhile (not . null . fst) $ iterate (g . fst) (s ++ "e", undefined)
    where f (a, b) (c, d) = (a + c, b + d)
          g ('e':s)     = (s, (1, 0))
          g ('s':'e':s) = (s, (1, -1))
          g ('s':'w':s) = (s, (0, -1))
          g ('w':s)     = (s, (-1, 0))
          g ('n':'w':s) = (s, (-1, 1))
          g ('n':'e':s) = (s, (0, 1))