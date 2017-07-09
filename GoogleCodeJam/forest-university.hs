-- 2016-09-29
-- forest university, code jam 2016
import Data.List
import System.Random
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy

main :: IO ()
main = getLine >>= \s -> forM_ [(1 :: Int) .. read s] $ \i -> solveM >>= \t -> putStrLn $ "Case #" ++ show i ++ ": " ++ t

rdLine :: IO [Int]
rdLine = fmap (map read . words) getLine

solveM :: IO String
solveM = do
  _ <- getLine
  parents <- rdLine
  chars <- fmap (V.fromList . (' ' :)) getLine
  _ <- getLine
  cools <- fmap words getLine
  let
    n = length parents
    children = V.accum (flip (:)) (V.replicate (succ n) []) $ zip parents [1..]
    hats = mkHats children chars
  return $ unwords $ map (show . solve 2000 hats) cools

solve :: Int -> [String] -> String -> Double
solve k hats cool = fromIntegral (length $ filter (cool `isInfixOf`) $ take k $ hats) / fromIntegral k

mkHats :: V.Vector [Int] -> V.Vector Char -> [String]
mkHats children chars = evalState loop (mkStdGen 10)
  where
  loop :: State StdGen [String]
  loop = do
    hat <- fmap tail $ go 0
    hats <- loop
    return (hat : hats)

  go :: Int -> State StdGen String
  go i = fmap ((chars V.! i) :) $ mapM go (children V.! i) >>= combine
  
combine :: [String] -> State StdGen String
combine ss = do
  is <- shuffle $ concat $ zipWith (\s i -> replicate (length s) i) ss [0..]
  return $ runST $ (V.thaw $ V.fromList ss) >>= combine' is

combine' :: [Int] -> M.MVector s String -> ST s String
combine' is v = mapM f is
  where
  f i = do
    s <- M.read v i
    M.write v i (tail s)
    return (head s)

shuffle :: [a] -> State StdGen [a]
shuffle xs = do
  is <- mapM (\i -> state $ randomR (0, i)) [n-1,n-2..0]
  return $ runST $ V.thaw (V.fromList xs) >>= grab is
  where
  n = length xs

grab :: [Int] -> M.MVector s a -> ST s [a]
grab is v = mapM (uncurry f) (zip [0..] is)
  where
  f i j = do
    M.swap v i (i + j)
    M.read v i
    
