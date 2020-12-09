import Data.List

main = do  
    ss <- fmap lines getContents
    print . sum . map (length . nub) $ f ss

f :: [String] -> [String]
f [] = [[]]
f ([]:ss) = ([] : f ss)
f (s:ss) = let ts = f ss in ((s ++ head ts) : tail ts)