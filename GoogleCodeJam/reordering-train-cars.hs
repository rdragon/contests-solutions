-- 2016-09-14
-- reordering train cars, code jam 2014
import Data.List
import Control.Monad

main :: IO ()
main = getLine >>= \s -> forM_ [1 .. read s] $ \(i :: Int) -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

solveM :: IO String
solveM = do
  _ <- getLine
  ws <- fmap words getLine
  return $ show $ ways ws 'a'

ways :: [String] -> Char -> Int
ways ws c
  | c == succ 'z' = fac $ length ws
  | or $ map (\w -> (length $ filter (== c) w) >= 2) hits = 0
  | not (null mids) && not (null (fulls ++ lefts ++ rights)) = 0
  | length lefts >= 2 || length rights >= 2 = 0
  | null hits = ways ws (succ c)
  | otherwise = fac (length fulls) `mult` ways (newWord : rest) (succ c)
  where
  (hits, rest) = partition (c `elem`) $ map shorten ws
  (fulls, temp1) = partition ((== 1) . length) hits
  (lefts, temp2) = partition ((== c) . head) temp1
  (rights, mids) = partition ((== c) . last) temp2
  newWord = concat $ rights ++ fulls ++ lefts ++ mids

shorten :: String -> String
shorten = map head . group
  
modulo :: Int
modulo = 1000000007

mult :: Int -> Int -> Int
mult x y = (x * y) `mod` modulo

fac :: Int -> Int
fac 0 = 1
fac n = mult n $ fac (pred n)
