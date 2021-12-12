System.IO.File.ReadAllLines("input.txt")
|> Array.map (fun s -> s.Substring(61).Split(' '))
|> Array.concat
|> Array.map Seq.length
|> Array.filter (fun n -> n = 2 || n = 3 || n = 4 || n = 7)
|> Array.length
