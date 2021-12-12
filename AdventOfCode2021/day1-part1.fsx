System.IO.File.ReadAllLines("input.txt")
|> Array.map int
|> Array.pairwise
|> Array.filter (fun (i, j) -> j > i)
|> Array.length
