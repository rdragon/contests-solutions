// 2021-08-17
[for a in [2..100] do for b in [2..100] do bigint a ** b] |> Seq.distinct |> Seq.length