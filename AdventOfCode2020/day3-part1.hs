import Data.Char  
  
main = do  
    contents <- getContents
    let ss = map (concat . repeat) $ lines contents
    print (compute ss)

compute :: [String] -> Int
compute [] = 0
compute (s:ss) = (if head s == '#' then 1 else 0) + compute (map (drop 3) ss)