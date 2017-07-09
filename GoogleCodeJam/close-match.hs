-- 2016-06-18
import Control.Monad
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    [c, j] <- fmap words getLine
    putStrLn $ "Case #" ++ show i ++ ": " ++ (snd . head . sortOn fst $ f EQ c j [] [])
  where
    r = read :: String -> Int
    f _ [] _ x' y' = let {x = reverse x'; y = reverse y'} in [(abs (r y - r x), x ++ " " ++ y)]
    f GT (x:xs) (y:ys) s t = f GT xs ys (mn x : s) (mx y : t)
    f LT (x:xs) (y:ys) s t = f LT xs ys (mx x : s) (mn y : t)
    f EQ ('?':xs) ('?':ys) s t = concatMap (\[x, y] -> f EQ (x:xs) (y:ys) s t) ["00", "01", "10"]
    f EQ ('?':xs) (y:ys) s t = concatMap (\x -> f EQ (x:xs) (y:ys) s t) [max '0' (pred y) .. min '9' (succ y)]
    f EQ (x:xs) ('?':ys) s t = concatMap (\y -> f EQ (x:xs) (y:ys) s t) [max '0' (pred x) .. min '9' (succ x)]
    f EQ (x:xs) (y:ys) s t = f (x `compare` y) xs ys (x:s) (y:t)
    f _ _ _ _ _ = undefined
    mn '?' = '0'
    mn x = x
    mx '?' = '9'
    mx x = x
