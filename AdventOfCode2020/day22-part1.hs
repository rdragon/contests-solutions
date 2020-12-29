import Text.Read
import Data.Maybe

main = interact $ show . f . (\xs -> splitAt (length xs `div` 2) xs) . catMaybes . map readMaybe . lines
    where f (xs, ys) | null xs || null ys = sum . zipWith (*) [1..] . reverse $ xs ++ ys
          f (x:xs, y:ys) = f $ if x > y then (xs ++ [x, y], ys) else (xs, ys ++ [y, x])