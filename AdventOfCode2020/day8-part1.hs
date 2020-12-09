main = do  
    ts <- fmap lines getContents
    print $ run ts 0 0 []

run :: [String] -> Int -> Int -> [Int] -> Int
run ts j acc js
    | j `elem` js = acc
    | inst == "nop" = run ts (succ j) acc js'
    | inst == "acc" = run ts (succ j) (acc + num) js'
    | otherwise = run ts (j + num) acc js'
    where
        [inst, num'] = words $ ts !! j
        num = read $ dropWhile (== '+' )num' :: Int
        js' = (j:js)
