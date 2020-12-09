import Data.List

main = do  
    ss <- fmap lines getContents
    print . sum . map length $ f ss

f :: [String] -> [String]
f [] = [['a'..'z']]
f ([]:ss) = (['a'..'z'] : f ss)
f (s:ss) = let ts = f ss in ((intersect s $ head ts) : tail ts)