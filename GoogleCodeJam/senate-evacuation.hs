-- 2016-06-21
import Data.List
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B
import Control.Monad
import qualified Data.Set as Set

main :: IO ()
main = do
  ints <- intReader
  [t] <- ints 1
  forM_ [1..t] $ \i -> do
    [n] <- ints 1
    s <- fmap (foldr Set.insert Set.empty . flip zip ['A'..]) $ ints n
    putStrLn $ "Case #" ++ show i ++ ": " ++ (intercalate " " $ solve s Nothing)
  where
  solve s c_mb
    | null s = []
    | Set.size s == 2 && isNothing c_mb = solve t (Just c)
    | otherwise = maybe [c] (: [c]) c_mb : solve t Nothing
    where
    (n, c) = Set.findMax s
    t = (if n == 1 then id else Set.insert (pred n, c)) . Set.delete (n, c) $ s

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
