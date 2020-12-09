main = do  
    ts <- fmap lines getContents
    print $ run ts 0 0 [] 0

run :: [String] -> Int -> Int -> [Int] -> Int -> Int
run ts j acc js k
    | j == length ts = acc
    | j `elem` js = run ts 0 0 [] (succ k)
    | inst == "acc" = run ts (succ j) (acc + num) js' k
    | (inst == "nop") == (j /= k) = run ts (succ j) acc js' k
    | otherwise = run ts (j + num) acc js' k
    where
        [inst, num'] = words $ ts !! j
        num = read $ dropWhile (== '+' )num' :: Int
        js' = (j:js)
