// 2021-08-21
// Optimized version using the Miller-Rabin test.
// This nice test was mention in the problem forum.

let isOdd x = x &&& 1L = 1L

let modPow x n m =
    if n = 0L then
        1L
    else
        let rec f x n m r =
            if n = 1L then
                (x * r % m)
            elif isOdd n then
                f (x * x % m) ((n - 1L) >>> 1) m (x * r % m)
            else
                f (x * x % m) (n >>> 1) m r

        f x n m 1L

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

let millerRabin =
    function
    | n' when n' > 2 ->
        let rec f s d p =
            if isOdd d then
                (s, d)
            else
                f (s + 1) (d >>> 1) (p <<< 1)

        let n = int64 n'
        let (s, d) = f 1 ((n - 1L) >>> 1) 2L

        let check a =
            if a >= n then
                true
            else
                let x = modPow a d n

                if x = 1L || x = n - 1L then
                    true
                else
                    iterate (fun x -> x * x % n) (x * x % n)
                    |> Seq.take (s - 1)
                    |> Seq.contains (n - 1L)

        check 2L && check 7L && check 61L
    | n -> n = 2

let isPrime (i, n) = i % 4 <> 0 && millerRabin n

seq { 2 .. 2 .. 100_000 }
|> Seq.collect ((+) >> Seq.replicate 4)
|> Seq.scan (fun x f -> f x) 1
|> Seq.indexed
|> Seq.scan (fun (x, y) t -> (x + (if isPrime t then 1 else 0), y + 1)) (0, 0)
|> Seq.skip 5
|> Seq.chunkBySize 4
|> Seq.map Seq.head
|> Seq.findIndex (fun (x, y) -> float x / float y < 0.1)
|> (fun x -> x * 2 + 3)
