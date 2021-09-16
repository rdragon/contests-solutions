// 2021-08-16
let maxP = 100_000
let sieve =
    let array = Array.replicate (maxP + 1) true
    let h p =
        if Array.get array p then
            for q in p * p .. p .. maxP do
                Array.set array q false
    seq { 2 .. maxP }
        |> Seq.takeWhile (fun p -> p * p <= maxP)
        |> Seq.iter h
    array
let isPrime n = if n > 1 then Array.get sieve n else false
let f a b = [0..100] |> List.takeWhile (fun n -> isPrime (n * n + n * a + b)) |> List.length
let g maxN =
    let bs = [2..maxN] |> List.filter isPrime
    let (a, b) = Seq.allPairs (seq { -maxN .. maxN }) bs |> Seq.maxBy (fun (a, b) -> f a b)
    printfn "n^2 + %i * n + %i (%i consecutive primes)" a b (f a b)
    a * b
g 1000