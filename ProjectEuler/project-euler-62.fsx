// 2021-08-21

[1 .. 10_000]
|> List.map (fun x -> pown (int64 x) 3)
|> List.groupBy (string >> Seq.sort >> Seq.toList)
|> List.map snd
|> List.filter (List.length >> ((=) 5))
|> List.map List.min
|> List.min