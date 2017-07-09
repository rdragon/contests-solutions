-- 2016-04-26
-- (deze oplossing dacht ik eerder aan, maar dacht dat ie te algemeen was. toch mooier dit. werkte meteen)

-- vertices are numbered as follows, from left to right, top to bottom: 7, 6, 5, 8, *, 4, 1, 2, 3

import Data.List

many [] = []
many s = parens s ++ "*"

choose [] = []
choose [s] = s
choose ss = parens $ intercalate "|" ss

parens s = '(' : s ++ ")"

possible v w a 
  | v == w = True
  | otherwise = w `elem` range v a

edges 1 = [('R', 2)]
edges 2 = [('R', 3), ('U', 8), ('L', 1)]
edges 3 = [('L', 2), ('T', 4)]
edges 4 = [('T', 3), ('U', 5)]
edges 5 = [('L', 6), ('D', 4)]
edges 6 = [('L', 7), ('P', 9)]
edges 7 = [('R', 6), ('J', 8)]
edges 8 = [('D', 2)]
edges 9 = []

-- neighbours
nb v = [u | (_, u) <- edges v]

-- returns regex that returns true iff path is from v to w, no internal vertices equal to w, and no internal vertices in a
f v w a
  | v `elem` a = choose [d : if u == w then "" else f u w a | (d, u) <- edges v, u == w || (u `notElem` a && possible u w a)]
  | otherwise = many (f v v (v:a)) ++ f v w (v:a)
  
-- all vertices w such that there exists a path from v to w with no internal vertices in a
range v a = v : go (nb v) [v]
  where
    -- go verticesToTry verticesTried
    go [] _ = []
    go (w:ws) b
      | w `elem` b = go ws b
      | w `elem` a = w : go ws (w:b)
      | otherwise = w : go (nb w ++ ws) (w:b)

main = print $ '^' : f 1 9 [] ++ f 6 1 [] ++ "$"