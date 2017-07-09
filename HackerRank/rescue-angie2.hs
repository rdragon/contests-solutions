-- 2016-04-26
-- vertices are numbered as follows, from left to right, top to bottom: 7, 6, 5, 8, *, 4, 1, 2, 3

import Data.List
import Data.Maybe

many :: String -> String
many [] = []
many s = parens s ++ "*"

choose :: [String] -> String
choose [s] = s
choose ss = parens $ intercalate "|" ss

parens :: String -> String
parens s = '(' : s ++ ")"

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
nb :: Int -> [Int]
nb v = [u | (_, u) <- edges v]

-- returns regex that returns true iff path is from v to w, no internal vertices equal to w, and no internal vertices in a
f :: Int -> Int -> [Int] -> Maybe String
f v w a
  | v `elem` a = if null rs then Nothing else Just (choose rs)
  | otherwise = if isJust r then Just (many (fromMaybe [] (f v v (v:a))) ++ fromJust r) else Nothing
  where rs = [d : if u == w then "" else fromJust r | (d, u) <- edges v, let r = f u w a, u == w || (u `notElem` a && isJust r)]
        r = f v w (v:a)
main :: IO ()
main = print $ '^' : fromJust (f 1 9 []) ++ fromJust (f 6 1 []) ++ "$"
