let rec f s t i j n =
    if t >= 1000 then
        s * n
    else
        let i = (i + (((n + 1) % 100) + 1) * 3) % 10
        f t (s + i + 1) j i (n + 3)

System.IO.File.ReadAllLines("input.txt")
|> Array.map (fun s -> s.Substring(28) |> int)
|> fun xs -> f 0 0 (xs[0] - 1) (xs[1] - 1) 0
