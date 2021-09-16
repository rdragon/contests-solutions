-- 2021-08-17
import Data.Char
main = print. sum $ filter f [2..354294]
f n = (== n). sum. map ((^ 5). digitToInt) $ show n