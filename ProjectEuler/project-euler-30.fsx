// 2021-08-17
// Note that 354_294 (= 6 * 9^5) is an upper bound on any such number.
let f n = string n |> Seq.sumBy (fun c -> pown (string c |> int) 5) |> ((=) n)
[2..354_294] |> Seq.filter f |> Seq.sum