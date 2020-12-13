import Data.Array.Unboxed
import Data.List

type Board = UArray (Int, Int) Char

main = do
    bs <- fmap (tails . iterate updateBoard . getBoard . lines) getContents
    print . length . filter (== '#') . elems $ head [b | (b:c:_) <- bs, b == c]

getBoard :: [String] -> Board
getBoard ss = listArray ((1, 1), (rows, cols)) (concat ss)
    where cols = length $ head ss
          rows = length ss

updateBoard :: Board -> Board
updateBoard b = listArray (bounds b) . map (updateSpot b) $ assocs b

updateSpot :: Board -> ((Int, Int), Char) -> Char
updateSpot b (p, c)
    | c == 'L' && null cs = '#'
    | c == '#' && (not . null $ drop 4 cs) = 'L'
    | otherwise = c
    where cs = filter (== '#') . map (b !) $ proximity (bounds b) p

proximity :: ((Int, Int), ((Int, Int))) -> (Int, Int) -> [(Int, Int)]
proximity bnds (y, x) = filter (inRange bnds) $ range ((y-1, x-1), (y+1, x+1))