let w, cs, q, p = 4_000_000, [|',';':';'=';' '|], List.sortBy (fun (x, _, _) -> x), fun i (x, y, _) -> if x > i then i else max i (y + 1)
let r = Seq.map (fun [x;y;i;j] -> let n = abs (x-i) + abs (y-j) - abs y in (x-n, x+n, y)) >> Seq.toList >> q
let us = System.IO.File.ReadAllLines("15") |> Seq.map (fun s -> let xs = s.Split(cs) in [for i in [3;6;13;16] -> int xs[i]]) |> r
let f m (x, y, z) = if m >= z then (x+1, y-1, z) else (x-1, y+1, z)
let g, h = (fun bs m -> List.map (f m) bs |> q), fun (y, x) -> int64 x * 4_000_000L + int64 y
[0..w] |> Seq.scan g us |> Seq.map (List.fold p 0) |> Seq.indexed |> Seq.find (fun (_, x) -> x <= w) |> h
