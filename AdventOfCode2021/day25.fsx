let move m n ps qs =
    let f ps di dj ((i, j) as p) =
        let q = ((i + di) % m, (j + dj) % n)
        if Set.contains q ps || Set.contains q qs then p else q
    let ps = ps |> Set.map (f ps 0 1)
    let qs = qs |> Set.map (f ps 1 0)
    (ps, qs)

let rec solve m n ps qs u =
    let (ps', qs') = move m n ps qs
    if ps = ps' && qs = qs' then u else solve m n ps' qs' (u + 1)

System.IO.File.ReadAllLines("input.txt")
|> fun ss ->
    let m = Array.length ss
    let n = ss[0].Length
    let f c =
        [ for i in 0 .. m - 1 do
            for j in 0 .. n - 1 do
                if ss[i][j] = c then (i, j) ]
        |> Set.ofList
    solve m n (f '>') (f 'v') 1
