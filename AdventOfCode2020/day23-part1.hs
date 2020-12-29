import Data.List

main = interact $ take 9 . tail . dropWhile (/= '1') . cycle . (!! 100) . iterate f

f (a:b:c:d:es) = is ++ [e, b, c, d] ++ js ++ [a]
    where Just e = find (`elem` es) $ iterate g (g a)
          (is, (_:js)) = span (/= e) es

g '1' = '9'
g x = pred x