// 2021-08-18
// All 9 and 8 digit pandigitals are divisible by 3, so the number we're looking for has 7 digits.
let isPrime n = seq { 2 .. n } |> Seq.takeWhile (fun x -> x * x <= n) |> Seq.forall (fun x -> n % x <> 0)
let rec f cs =
    if List.length cs = 7 then
        let n = cs |> List.rev |> List.toArray |> System.String |> int
        if isPrime n then seq { n } else Seq.empty
    else "7654321" |> Seq.except cs |> Seq.collect (fun c -> f (c :: cs))
f List.empty |> Seq.head