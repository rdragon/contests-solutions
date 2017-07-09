-- 2016-08-21
import Data.List
import Control.Monad

main :: IO ()
main = do
  testC <- fmap read getLine :: IO Int
  forM_ [1..testC] $ \testIx -> do
    [_, _, s] <- fmap (map read . words) getLine :: IO [Int]
    keys <- getLine
    word <- getLine
    let ans = solve s keys word
    putStrLn $ "Case #" ++ show testIx ++ ": " ++ show ans

solve :: Int -> String -> String -> Double
solve s keys word
  | not allCharsPresent = 0
  | otherwise = fromIntegral maxBananas - p * fromIntegral (s - l + 1)
  where
  k = length keys
  l = length word
  isPrefix x = and $ zipWith (==) x word
  overlap = head $ dropWhile (not . isPrefix) $ drop 1 $ tails word
  allCharsPresent = all (\c -> c `elem` keys) word
  maxBananas = (s - length overlap) `quot` (l - length overlap)
  getKeyProb c = fromIntegral (length (filter (== c) keys)) / fromIntegral k :: Double
  p = product $ map getKeyProb word