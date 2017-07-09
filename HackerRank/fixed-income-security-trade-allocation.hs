-- 2016-06-29
import Data.Function
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine
  triple <- fmap (map read . words) getLine
  pos <- fmap (sort . map ((\[x, y] -> (read y, x)) . words)) . sequence . replicate t $ getLine
  putStrLn (solve triple pos)

solve :: [Double] -> [(Double, String)] -> String
solve [mts, inc, au0] pos = intercalate "\n" . map (\(x, y) -> y ++ " " ++ show (round x :: Integer)) . sortBy (compare `on` snd) . run $ au0
  where
    totOrder = sum . map fst $ pos
    run :: Double -> [(Double, String)]
    run au = zip os (map snd pos)
      where
        os = reverse . (\(_, y, _) -> y) . foldl' f (au, [], totOrder) $ (map fst pos)
    f :: (Double, [Double], Double) -> Double -> (Double, [Double], Double)
    f (au, os, tot) o
      | p < mts && p > mts / 2 && check mts = order mts
      | p < mts = zero
      | p >= o = order o
      | check p' = order p'
      | otherwise = zero
      where
        p = o / tot * au
        zero = (au, 0:os, tot - o)
        order x = if x <= au then (au - x, x:os, tot - o) else zero
        p' = ((fromIntegral :: Integer -> Double) . floor $ ((p - mts) / inc)) * inc + mts
        check y = let x = ((o - y - mts) / inc) in (o - y >= mts) && x - fromIntegral (floor x :: Integer) == 0
solve _ _ = undefined