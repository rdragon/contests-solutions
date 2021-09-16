// 2021-08-16
let t = bigint 10 ** 999
let rec f a b i = if b >= t then i else f b (a + b) (i + 1)
f (bigint 1) (bigint 1) 2