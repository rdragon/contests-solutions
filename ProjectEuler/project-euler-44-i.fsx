// 2021-08-19

let numbers =
    [ 1 .. 10_000 ]
    |> List.scan (fun x y -> x + y * 3 + 1) 1

let numberSet = System.Collections.Generic.HashSet numbers

let f d =
    numbers
    |> Seq.tryFind
        (fun x ->
            numberSet.Contains(x + d)
            && numberSet.Contains(x + x + d))
    |> Option.isSome

numbers |> Seq.tryFind f
