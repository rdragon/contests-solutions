-- 2018-04-07
-- cubic ufo, code jam 2018, qualification round

-- this solution only solves the first dataset

import Text.Printf

solve :: Double -> String
solve t = printf "0 0 -0.5\n%f %f 0\n%f %f 0" (cos b / 2) (sin b / 2) (cos c / 2) (sin c / 2)
  where
  a = acos $ t / sqrt 2 :: Double
  b = a + pi / 4 :: Double
  c = a - pi / 4 :: Double

main :: IO ()
main = interact go
  where
  go s = unlines. map run. tail. zip [0..]. lines $ s
  run :: (Int, String) -> String
  run (k, line) = printf "Case #%d:\n%s" k (solve. read $ line)
