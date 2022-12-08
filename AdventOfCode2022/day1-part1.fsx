let f s = if s = "" then (*) 0 else (+) (int s)
System.IO.File.ReadAllLines "1" |> Seq.map f |> Seq.scan (|>) 0 |> Seq.max
