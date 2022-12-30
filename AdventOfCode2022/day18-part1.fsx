let us, f = System.IO.File.ReadAllLines("18") |> Array.map (fun s -> s.Split(',') |> Array.map int), fun (x, y) -> abs (x - y)
6 * us.Length - (Seq.allPairs us us |> Seq.filter (fun a -> a ||> Array.zip |> Array.sumBy f |> ((=) 1)) |> Seq.length)
