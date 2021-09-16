-- This solution was written on 2021-05-16 during a practice round.

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as UM

main :: IO ()
main = do
    n <- read <$> getLine
    sequence_ [runTest i | i <- [1..n]]

runTest :: Int -> IO ()
runTest i = do
    [n, a, b] <- map read . words <$> getLine
    uv <- U.fromList . map read . words <$> getLine
    ans <- solve n a b uv
    putStrLn $ "Case #" ++ show i ++ ": " ++ (maybe "IMPOSSIBLE" show ans)

ansMax :: Int
ansMax = 10000

solve :: Int -> Int -> Int -> U.Vector Int -> IO (Maybe Int)
solve n a b uv = do
    mv <- M.replicate ansMax undefined
    run mv 0
    where
        run :: M.IOVector (U.Vector Int) -> Int -> IO (Maybe Int)
        run _ i | i == pred ansMax = return Nothing
        run mv i = do
            v <- transmute n a b uv mv (i - 1) i
            if U.and (U.zipWith (>=) v uv) then
                return $ Just (i + 1)
            else
                run mv (i + 1)

transmute :: Int -> Int -> Int -> U.Vector Int -> M.IOVector (U.Vector Int) -> Int -> Int -> IO (U.Vector Int)
transmute n a b uv mv maxIndex index = do
    if index < 0 then
        return $ U.replicate n 0
    else if index <= maxIndex then
        M.read mv index
    else do
        v <- run
        M.write mv index v
        return v
    where
        run :: IO (U.Vector Int)
        run
            | index < n && uv U.! index > 0 = return $ U.generate n (\i -> if i == index then 1 else 0)
            | otherwise = do
            v' <- transmute n a b uv mv maxIndex (index - a)
            v <- U.zipWith (+) v' <$> transmute n a b uv mv maxIndex (index - b)
            mv' <- U.thaw v
            subtransmute n a b uv mv'
            U.freeze mv'
  
subtransmute :: Int -> Int -> Int -> U.Vector Int -> UM.IOVector Int -> IO ()
subtransmute n a b uv mv = sequence_ [run i | i <- [n - 1, n - 2 .. 0]]
    where
        run :: Int -> IO ()
        run i = do
            x <- UM.read mv i
            let y = uv U.! i
            if x <= y then
                return ()
            else do
                let z = x - y
                up (i - a) z
                up (i - b) z
                up i (-z)
        
        up :: Int -> Int -> IO ()
        up i _ | i < 0 = return ()
        up i x = UM.modify mv (+ x) i
