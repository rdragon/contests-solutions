-- 2016-06-18
import Control.Monad
import Data.List

main :: IO ()
main = do
  t <- fmap read getLine :: IO Int
  forM_ [1..t] $ \i -> do
    s <- getLine
    let
      a 6 = c 'X'
      a 0 = c 'Z'
      a 4 = c 'U'
      a 8 = c 'G'
      a 2 = c 'W'
      a 5 = c 'F' - a 4
      a 3 = c 'H' - a 8
      a 9 = c 'I' - a 5 - a 8 - a 6
      a 7 = c 'V' - a 5
      a 1 = c 'N' - a 7 - 2 * a 9
      a _ = undefined
      c x = length . filter (== x) $ s
    putStrLn $ "Case #" ++ show i ++ ": " ++ (sort . concatMap (\j -> replicate (a j) (head (show j))) $ [0..9 :: Int])

    
