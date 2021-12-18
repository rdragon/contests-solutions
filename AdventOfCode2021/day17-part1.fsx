System.IO.File.ReadAllText("input.txt")
|> (fun s -> (s.Split('-')[1]).TrimEnd('.') |> int)
|> (fun x -> x * (x - 1) / 2)
