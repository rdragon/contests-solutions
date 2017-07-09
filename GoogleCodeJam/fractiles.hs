-- 2016-07-10
import Data.List
import Control.Monad

main :: IO ()
main = do
  testc <- fmap read getLine :: IO Int
  forM_ [1..testc] $ \testn -> do
    [k, cIn, s] <- fmap (map read . words) getLine :: IO [Int]
    let
      go :: Int -> [Int] -> Maybe [Int]
      go i ps
        | length ps > s = Nothing
        | i >= k = Just ps
        | otherwise = f ps i 0 0
      f :: [Int] -> Int -> Int -> Int -> Maybe [Int]
      f ps i c p
        | c == cIn = go i (p:ps)
        | otherwise = f ps (succ i) (succ c) (p * k + (i `mod` k))
      ans = case go 0 [] of
        Nothing -> "IMPOSSIBLE"
        Just ps -> intercalate " " $ map (show . succ) ps
    putStrLn $ "Case #" ++ show testn ++ ": " ++ ans
