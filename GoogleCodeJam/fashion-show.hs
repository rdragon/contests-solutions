-- 2017-04-08
-- fashion-show, code jam 2017
-- compiled with GHC 8.0.1
-- works with small dataset only
import Control.Monad
import Text.Printf
import Data.List

data Model = Model { m_t :: Char, _m_r :: Int, m_c :: Int }

instance Show Model where show (Model t r c) = printf "%c %d %d" t r c

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i ->
    solve >>= \(x, y, xs) -> do
      putStrLn $ printf "Case #%d: %d %d" i x y
      mapM_ putStrLn xs

solve :: IO (Int, Int, [String])
solve = do
  [n, mc] <- map read. words <$> getLine :: IO [Int]
  ms <- replicateM mc $ fmap ((\[t, r, c] -> Model (head t) (read r) (read c)). words) getLine :: IO [Model]
  let qs = make n ms $ find ((/= '+'). m_t) ms
  return (if n == 1 then 2 else 3 * n - 2, length qs, map show qs)
  where
  make n ms mb = ms1 ++ ms2 ++ ms3 ++ ms4
    where
    ms1 = [Model '+' n c | c <- [2..n-1]]
    ms2 = [Model '+' 1 c | c <- [1..n], c /= oc, c `notElem` map m_c ms]
    ms3 = case mb of
      Just (Model 'o' _ _) -> []
      _ -> [Model 'o' 1 oc]
    ms4 = [Model 'x' r c | (r, c) <- zip [2..n] $ filter (/= oc) $ [2..n] ++ [1]]
    oc = maybe 1 m_c mb
