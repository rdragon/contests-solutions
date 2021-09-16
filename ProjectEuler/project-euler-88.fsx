// 2021-08-24

let maxN = 30_000

let someDivisors n =
    let m = System.Math.Sqrt(float n) |> int
    [ 2 .. m ] |> Seq.filter (fun x -> n % x = 0)

let add ss n =
    let getSet m = Map.find m ss

    // The set that contains all integers i such that n is a product sum number with k = n - i (k = 1 is allowed).
    let s =
        someDivisors n
        |> Seq.collect (fun d -> getSet (n / d) |> Seq.map ((+) (d - 1)))
        |> Set.ofSeq
        |> Set.add (n - 1)

    Map.add n s ss

let data =
    [ 2 .. maxN ]
    |> Seq.fold add Map.empty
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.toList

let findNumber =
    function
    | 2 -> 4
    | 3 -> 6
    | 4 -> 8
    | 5 -> 8
    | 6 -> 12
    | k ->
        data
        |> Seq.find (fun (n, s) -> n > k && Set.contains (n - k) s)
        |> fst

let solve n =
    [ 2 .. n ]
    |> Seq.map findNumber
    |> Seq.distinct
    |> Seq.sum

solve 12_000
