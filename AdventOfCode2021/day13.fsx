let g (s: string) =
    s.Split(',')
    |> Array.map int
    |> (fun xs -> (xs[0], xs[1]))

let f ps (s: string) =
    let ss = s.Split('=')
    let n = int ss[1]
    let g c x = if Seq.last ss[0] = c && x > n then 2 * n - x else x
    ps
    |> Seq.map (fun (x, y) -> (g 'x' x, g 'y' y))
    |> Seq.distinct

let h (ss, ss') =
    f (Seq.map g ss') (Seq.head ss)
    |> Seq.length

let h' (ss, ss') =
    let cs = Array.replicate 280 ' '
    ss
    |> Seq.fold f (Seq.map g ss')
    |> Seq.iter (fun (x, y) -> Array.set cs (x + y * 40) '#')
    cs
    |> Array.chunkBySize 40
    |> Array.iter (System.String >> printfn "%s")

System.IO.File.ReadAllLines("input.txt")
|> Array.filter ((<>) "")
|> Array.partition (fun s -> s[0] = 'f')
|> (fun t -> (h t, h' t))
