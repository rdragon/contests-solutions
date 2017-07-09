-- 2016-09-08
-- full binary tree, code jam 2014
import Data.Tuple
import Data.List
import qualified Data.Vector as V
import Control.Monad
import Data.Maybe
import Data.IORef
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
  rd <- intReader
  [t] <- rd 1
  forM_ [1 .. t] $ \i -> solveM rd >>= \s -> putStrLn $ "Case #" ++ show i ++ ": " ++ s

solveM :: (Int -> IO [Int]) -> IO String
solveM rd = do
  [n] <- rd 1
  xs <- fmap concat $ replicateM (pred n) $ fmap (\[x, y] -> [(pred x, [pred y]), (pred y, [pred x])]) $ rd 2
  let
    es = V.accum (++) (V.replicate n []) xs
  return $ show $ minimum $ map (\i -> getCost es i []) [0..pred n]

getCost :: V.Vector [Int] -> Int -> [Int] -> Int
getCost es i js = case length children of
  0 -> 0
  1 -> getWeight es (head children) i
  _ -> sum $ map fst a ++ map snd b
  where
  children = (es V.! i) \\ js
  (a, b) = splitAt 2 $ sortOn (\(c, w) -> c - w) $ map (\j -> (getCost es j [i], getWeight es j i)) children

getWeight :: V.Vector [Int] -> Int -> Int -> Int
getWeight es i j = 1 + (sum $ map (\k -> getWeight es k i) $ filter (/= j) (es V.! i))

intReader :: IO (Int -> IO [Int])
intReader = do
  ws <- fmap ((concatMap B.words) . B.lines) B.getContents >>= newIORef
  return $ \n -> do
    xs <- readIORef ws
    writeIORef ws (drop n xs)
    return (take n . map (fst . fromJust . B.readInt) $ xs)
