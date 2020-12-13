import Data.Array.Unboxed
import Data.List
import Data.Maybe

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
    where cs = filter id . map (maybe False (== '#') . listToMaybe . dropWhile (== '.') . map (b !)) $ proximity (bounds b) p

proximity :: ((Int, Int), ((Int, Int))) -> (Int, Int) -> [[(Int, Int)]]
proximity bnds p = [takeWhile (inRange bnds) . tail $ iterate (step q) p | q <- range ((-1, -1), (1, 1)), q /= (0, 0)]
    where step (y, x) (y', x') = (y + y', x + x')