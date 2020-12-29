import qualified Data.Vector.Unboxed.Mutable as M

main = do
    xs <- fmap (map read . words . map (\c -> if c == ',' then ' ' else c)) getContents
    v <- M.replicate 30000000 0
    sequence_ $ zipWith (M.write v) xs [1..]
    print =<< (run v (length xs) $ last xs)

run :: M.IOVector Int -> Int -> Int -> IO Int
run _ 30000000 n = return n
run v t n = do
    m <- (\s -> if s == 0 then 0 else t - s) <$> M.read v n
    M.write v n t
    run v (t + 1) m