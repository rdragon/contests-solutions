type D = System.Collections.Generic.SortedDictionary<int * int * int * int * bool, bigint>

let rec f (d: D) m n =
    if d.Count = 0 then
        max m n
    else
        let (s, t, i, j, b) as p = Seq.head d.Keys
        let x = d[p]
        d.Remove(p) |> ignore
        if max s t > 20 then
            let (m, n) = if s > t then (m + x, n) else (m, n + x)
            f d m n
        else
            [ (3, 1I); (4, 3I); (5, 6I); (6, 7I); (7, 6I); (8, 3I); (9, 1I) ]
            |> Seq.iter (fun (k, y) ->
                let (i, j) = if b then ((i + k) % 10, j) else (i, (j + k) % 10)
                let (s, t) = if b then (s + i + 1, t) else (s, t + j + 1)
                let p = (s, t, i, j, not b)
                d[p] <- x * y + if d.ContainsKey(p) then d[p] else 0I)
            f d m n

System.IO.File.ReadAllLines("input.txt")
|> Array.map (fun s -> s.Substring(28) |> int)
|> fun xs ->
    let d = new D()
    d[(0, 0, xs[0] - 1, xs[1] - 1, true)] <- 1I
    f d 0I 0I
