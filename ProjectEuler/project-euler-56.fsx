// 2021-08-20

[ for a in [ 1 .. 99 ] do
      for b in [ 1 .. 99 ] do
          (bigint a) ** b
          |> string
          |> Seq.sumBy (string >> int) ]
|> List.max
