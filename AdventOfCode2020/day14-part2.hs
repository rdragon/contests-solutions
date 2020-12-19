import Data.List
import Text.Printf
import qualified Data.Map.Strict as M

main = do
    (_, mem) <- fmap (foldl' run (undefined, M.empty) . lines) getContents
    print . sum $ M.elems mem

run :: (String, M.Map Integer Int) -> String -> (String, M.Map Integer Int)
run (mask, mem) line
    | "mask = " `isPrefixOf` line = (reverse $ drop 7 line, mem)
    | otherwise = (mask, foldl' (\m a -> M.insert a value m) mem addresses)
    where addresses = f $ zipWith (\a b -> if a == '0' then b else a) mask original
          value = read . drop 2 $ dropWhile (/= '=') line
          original = reverse $ printf "%036b" (read . takeWhile (/= ']') $ drop 4 line :: Integer)

f :: String -> [Integer]
f [] = [0]
f (c:cs) = case c of 'X' -> xs ++ xs'
                     '0' -> xs
                     _ -> xs'
    where xs = map (*2) $ f cs
          xs' = map (+1) xs