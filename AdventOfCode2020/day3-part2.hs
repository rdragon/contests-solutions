import Data.Char  
  
main = do  
    contents <- getContents
    let ss = map (concat . repeat) $ lines contents
    let xs = map (compute ss) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print (foldl (*) 1 xs)

compute :: [String] -> (Int, Int) -> Int
compute [] _ = 0
compute ss@(s:_) (right, down) = (if head s == '#' then 1 else 0) + compute (map (drop right) (drop down ss)) (right, down)