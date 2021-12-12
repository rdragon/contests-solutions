let f b (us: int []) =
    let g =
        function
        | 0 -> (0, 1)
        | x -> (x / abs x, abs x)
    let (dx, m) = g (us[2] - us[0])
    let (dy, n) = g (us[3] - us[1])
    if b && min m n > 1 then
        Seq.empty
    else
        Seq.init (max m n + 1) (fun i -> (us[0] + dx * i, us[1] + dy * i))

let g b =
    System.Text.RegularExpressions.Regex.Split(System.IO.File.ReadAllText("input.txt").Trim(), "[^0-9]+")
    |> Array.map int
    |> Array.chunkBySize 4
    |> Seq.map (f b)
    |> Seq.concat
    |> Seq.groupBy id
    |> Seq.map snd
    |> Seq.filter (Seq.tail >> Seq.isEmpty >> not)
    |> Seq.length

(g true, g false)
