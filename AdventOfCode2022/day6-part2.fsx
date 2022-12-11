System.IO.File.ReadAllText("6") |> Seq.windowed 14 |> Seq.takeWhile (Set.ofSeq >> Set.count >> (>) 14) |> Seq.length |> (+) 14
