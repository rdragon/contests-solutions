// 2021-08-19

let isPrime n =
    n > 1
    && seq { 2 .. n }
       |> Seq.takeWhile (fun x -> x * x <= n)
       |> Seq.forall (fun x -> n % x <> 0)

let doubledSquares =
    [ for i in 1 .. 10_000 do
          2 * i * i ]

let f n =
    not (isPrime n)
    && doubledSquares
       |> Seq.forall (fun x -> x > n || not (isPrime (n - x)))

seq { 3 .. 2 .. 10_000 } |> Seq.tryFind f
