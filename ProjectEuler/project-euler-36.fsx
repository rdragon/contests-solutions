// 2021-08-17
let f (x : int) (b : int) = System.Convert.ToString(x, b) |> Seq.toList |> (fun s -> s = List.rev s)
[1..1_000_000] |> List.filter (fun x -> f x 2 && f x 10) |> List.sum