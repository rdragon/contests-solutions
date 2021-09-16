// 2021-08-18
let xs = seq { 0..1_000_000 } |> Seq.collect string
[for i in [1..7] do Seq.item (pown 10 i / 10) xs |> string |> int] |> Seq.reduce (*)