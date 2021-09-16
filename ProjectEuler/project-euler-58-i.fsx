// 2021-08-20

let primes =
    let rec f ps x =
        let ps' =
            if List.forall (fun p -> x % p <> 0) ps then
                (x :: ps)
            else
                ps

        if x > 40_000 then
            List.rev ps'
        else
            f ps' (x + 2)

    f [ 2 ] 3

let isPrime (i, n) =
    i % 4 <> 0
    && primes
       |> Seq.takeWhile (fun p -> p * p <= n)
       |> Seq.forall (fun x -> n % x <> 0)

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
