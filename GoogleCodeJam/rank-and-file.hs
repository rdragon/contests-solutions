-- 2016-06-15
import Control.Monad
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine
  forM_ [1..t :: Int] $ \i -> do
    n <- fmap read getLine
    ss <- sequence . replicate (2 * n - 1) . fmap (map read . words) $ getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ (intercalate " " . map show $ solve (map (\x -> (x, x)) ss) ss)

solve :: [([Int], [Int])] -> [[Int]] -> [Int]
solve ps ss
  | length ps1 == 1 = sort . ((s !! ix) :) . foldl' (flip delete) (map (!! ix) ss) $ s
  | otherwise = solve (map (fmap tail) ps2) ss
  where
    n = length $ head ss
    h = minimum . map (head . snd) $ ps
    (ps1, ps2) = partition ((== h) . head . snd) ps
    ix = n - (length . snd . head $ ps)
    s = fst . head $ ps1