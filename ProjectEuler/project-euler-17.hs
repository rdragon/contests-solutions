-- 2021-08-16
main = print . length . filter (/= ' ') $ concat [f i | i <- [1..1000]]
f 1 = "one"
f 2 = "two"
f 3 = "three"
f 4 = "four"
f 5 = "five"
f 6 = "six"
f 7 = "seven"
f 8 = "eight"
f 9 = "nine"
f 10 = "ten"
f 11 = "eleven"
f 12 = "twelve"
f 13 = "thirteen"
f 15 = "fifteen"
f 18 = "eighteen"
f 20 = "twenty"
f 30 = "thirty"
f 40 = "forty"
f 50 = "fifty"
f 60 = "sixty"
f 70 = "seventy"
f 80 = "eighty"
f 90 = "ninety"
f 1000 = "one thousand"
f n | n < 20 = f (n `mod` 10) ++ "teen"
f n | n < 100 = let (a, b) = n `divMod` 10 in f (a * 10) ++ " " ++ f b
f n = f a ++ " hundred" ++ t
    where
        (a, b) = n `divMod` 100
        t = if b == 0 then "" else " and " ++ f b