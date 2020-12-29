import Data.List
import Control.Monad
import Data.Vector.Unboxed (thaw, generate)
import qualified Data.Vector.Unboxed.Mutable as M

cups = 1000000

main = do
    m <- getContents >>= rd
    sequence_ . replicate 10000000 $ move m
    a <- M.read m 1
    b <- M.read m a
    print $ a * b

rd :: String -> IO (M.IOVector Int)
rd s = thaw $ generate (cups + 1) f
    where as = (10:) . reverse $ map (read . (:[])) s
          f a | a == cups || a == 0 = last as
          f a | a < 10 = last $ takeWhile (/= a) as
          f a = succ a

move :: M.IOVector Int -> IO ()
move m = do
    a <- g 0; b <- g a; c <- g b; d <- g c; e <- g d
    let Just i = find (`notElem` [b, c, d]) $ iterate f (f a)
    j <- M.read m i
    h a e >> h i b >> h d j >> h 0 e
    where f 1 = cups
          f i = pred i
          g = M.read m
          h = M.write m