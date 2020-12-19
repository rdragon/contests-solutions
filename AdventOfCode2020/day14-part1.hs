import Data.List
import Text.Printf

main = do
    (_, pairs) <- fmap (foldl' run (undefined, []) . lines) getContents
    print . sum $ map snd pairs

run :: (String, [(Integer, Integer)]) -> String -> (String, [(Integer, Integer)])
run (mask, pairs) line
    | "mask = " `isPrefixOf` line = (drop 7 line, pairs)
    | otherwise = (mask, ((address, value) : filter ((/= address) . fst) pairs))
    where address = read . takeWhile (/= ']') $ drop 4 line
          value = foldl' (\n c -> n * 2 + if c == '0' then 0 else 1) 0 $ zipWith (\a b -> if a == 'X' then b else a) mask original
          original = printf "%036b" (read . drop 2 $ dropWhile (/= '=') line :: Integer)