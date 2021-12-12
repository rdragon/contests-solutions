System.IO.File.ReadAllLines("input.txt")
|> Array.map int
|> Array.windowed 4
|> Array.filter (fun xs -> xs[3] > xs[0])
|> Array.length
