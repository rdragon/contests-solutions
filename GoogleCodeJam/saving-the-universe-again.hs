-- 2018-04-07
-- saving the universe again, code jam 2018, qualification round
import Data.List
import Text.Printf

damage :: String -> Int
damage xs = go 1 xs
  where
  go _ [] = 0
  go n ('S' : xs) = n + go n xs
  go n ('C' : xs) = go (2 * n) xs

minDamage :: String -> Int
minDamage = damage. reverse. sort

solve :: String -> Int -> Maybe Int
solve p d
  | d < minDamage p = Nothing
  | otherwise = Just. length. takeWhile (d <). map damage. iterate hack $ p

hack :: String -> String
hack = reverse. go. reverse
  where
  go [] = []
  go ('S' : 'C' : xs) = "CS" ++ xs
  go (x : xs) = x : go xs

main :: IO ()
main = interact go
  where
  go s = unlines. map run. tail. zip [0..]. lines $ s
  run :: (Int, String) -> String
  run (k, line) = printf "Case #%d: %s" k $ maybe "IMPOSSIBLE" show $ solve p (read q)
    where [q, p] = words line
