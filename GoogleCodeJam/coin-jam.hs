-- 2016-07-17
import Data.List

jamcoins :: [String]
jamcoins = map (\s -> '1' : (s ++ "1")) $ go 0 0 0 [] where
  go :: Int -> Int -> Int -> String -> [String]
  go 5 5 30 s = [s]
  go _ _ 30 _ = []
  go i j n s
    | even n && j < 5 = go i j1 n1 ('1':s) ++ go i j n1 ('0':s)
    | odd n  && i < 5 = go i1 j n1 ('1':s) ++ go i j n1 ('0':s)
    | otherwise =                             go i j n1 ('0':s)
    where
    i1 = succ i
    j1 = succ j
    n1 = succ n

main :: IO ()
main = putStrLn $ "Case #1:\n" ++ (intercalate "\n" $ map (\s -> s ++ " 3 2 3 2 7 2 3 2 3") $ take 500 jamcoins)


-- andere versie, na lezen van uitleg
import Control.Monad
import Data.List

main :: IO ()
main = do
    let coins = map (\x -> x ++ x) $ map (\x -> "1" ++ x ++ "1") $ sequence $ replicate 14 $ "01"
    let suffix = intercalate " " $ map show $ map (\b -> b ^ (16 :: Int) + 1) $ [(2 :: Int) .. 10]
    putStrLn "Case #1:"
    sequence_ $ map (\x -> putStrLn $ x ++ " " ++ suffix) $ take 500 $ coins