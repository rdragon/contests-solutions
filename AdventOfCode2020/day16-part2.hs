import Data.Char
import Data.List
import Data.Maybe

main = do
    s <- getContents
    print . f $ lines s

f :: [String] -> Integer
f ss = foldl1 (*) . map fst . filter snd . fromJust $ h rules fields
    where (ts, rs) = span (not . null) ss
          rules' = map (\s -> (g s, ("departure" `isPrefixOf` s))) ts
          rules = sortOn (\(rule, _) -> length $ filter (all (isValid rule)) fields) rules'
          fields = transpose . filter (all (\x -> any (flip isValid x) $ map fst rules')) . filter (not . null) $ map g rs
          isValid [a, b, c, d] x = a <= x && x <= b || c <= x && x <= d
          g s = map read . words $ map (\c -> if isDigit c then c else ' ') s
          h [] _ = Just []
          h ((rule, b) : rules) fields = listToMaybe $ catMaybes
              [fmap (\rest -> ((head field, b) : rest)) $ h rules (delete field fields) | field <- fields, all (isValid rule) field]