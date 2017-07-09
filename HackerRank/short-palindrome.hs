-- 2016-07-24
-- v: aantal keer dat elke letter is voorgekomen (we gaan de string van links naar rechts af). w: voor elke string van lengte twee, het aantal keer dat deze string is voorgekomen. u: het aantal keer dat xyy is voorgekomen voor een vaste willekeurige x (de precieze y maakt niet uit, we nemen de som over alle mogelijke y). ans: aantal keer dat xyyx is voorgekomen voor willekeurige x en y

import Data.IORef
import Control.Monad
import Data.List
import Data.Char
import qualified Data.Vector.Unboxed.Mutable as M

main :: IO ()
main = do
  s <- fmap (map (\c -> ord c - ord 'a')) getLine
  v <- M.replicate 26 (0 :: Int)
  w <- M.replicate (26 * 26) 0
  u <- M.replicate 26 0
  ans <- newIORef 0
  let
    go a = do
      M.read u a >>= (\x -> modifyIORef ans (plus x))
      forM_ [0..25] $ \b -> do
        x <- M.read w (b + a * 26)
        M.modify u (plus x) b
      forM_ [0..25] $ \b -> do
        x <- M.read v b
        M.modify w (plus x) (b + a * 26)
      M.modify v succ a
  mapM_ go s
  x <- readIORef ans
  print x
  where
  plus x y = (x + y) `mod` 1000000007