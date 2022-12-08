let rec f x = function | [] -> [x] | (s :: ss) -> if s = "" then (x :: f 0 ss) else f (x + int s) ss
System.IO.File.ReadAllLines "1" |> List.ofArray |> f 0 |> Seq.sortDescending |> Seq.take 3 |> Seq.sum
