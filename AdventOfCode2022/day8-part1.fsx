let f = Seq.map (Seq.mapFold (fun x (y, b) -> ((y, b || y > x), max x y)) 0 >> fst)
let g = Seq.transpose >> f >> Seq.map Seq.rev >> f
System.IO.File.ReadAllLines("8") |> Seq.map (Seq.map (fun c -> (int c, false))) |> g |> g |> Seq.collect id |> Seq.where snd |> Seq.length
