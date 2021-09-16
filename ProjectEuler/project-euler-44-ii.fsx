// 2021-08-19
// Using some float arithmetic seems to be a bit faster then using a hashset.

let numbers =
    [ 1 .. 10_000 ]
    |> List.scan (fun x y -> x + y * 3 + 1) 1

let isPent x =
    let y =
        (System.Math.Sqrt(1.0 + 24.0 * float x) + 1.0)
        / 6.0 in y = System.Math.Truncate(y)

let f d =
    numbers
    |> List.tryFind (fun x -> isPent (x + d) && isPent (x + x + d))
    |> Option.isSome

numbers |> List.tryFind f
