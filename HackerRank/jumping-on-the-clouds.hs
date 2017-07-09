-- 2016-04-29
main = do
  getLine
  c <- fmap (fmap read . words) getLine
  print (f (tail c))
f [] = 0
f [_] = 1
f [_, _] = 1
f (_:0:cs) = 1 + f cs
f (_:_:_:cs) = 2 + f cs