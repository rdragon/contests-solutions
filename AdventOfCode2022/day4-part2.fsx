let f [| a; b; c; d |] = a <= d && b >= c || c <= b && d >= a
System.IO.File.ReadAllLines "4" |> Seq.filter (fun s -> s.Replace(',', '-').Split('-') |> Array.map int |> f) |> Seq.length
