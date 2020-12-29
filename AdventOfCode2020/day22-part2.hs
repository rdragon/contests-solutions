import Text.Read
import Data.Maybe
import qualified Data.Set as S

main = interact $ show . snd . f S.empty . (\xs -> splitAt (length xs `div` 2) xs) . catMaybes . map readMaybe . lines
    where f _ ([], xs) = (False, g xs)
          f _ (xs, []) = (True, g xs)
          f ps p | p `S.member` ps = (True, undefined)
          f ps p@(x:xs, y:ys) | x > length xs || y > length ys = h ps p (x > y)
          f ps p@(x:xs, y:ys) = h ps p . fst $ f S.empty (take x xs, take y ys)
          g = sum . zipWith (*) [1..] . reverse
          h ps p@(x:xs, y:ys) b = f (S.insert p ps) $ if b then (xs ++ [x, y], ys) else (xs, ys ++ [y, x])