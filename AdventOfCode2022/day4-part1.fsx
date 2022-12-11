let f [| a; b; c; d |] = a >= c && b <= d || c >= a && d <= b
System.IO.File.ReadAllLines("4") |> Seq.filter (fun s -> s.Replace(',', '-').Split('-') |> Array.map int |> f) |> Seq.length
