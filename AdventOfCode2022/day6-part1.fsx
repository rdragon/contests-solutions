System.IO.File.ReadAllText("6") |> Seq.windowed 4 |> Seq.takeWhile (Set.ofSeq >> Set.count >> (>) 4) |> Seq.length |> (+) 4
