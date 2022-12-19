let p ([|x; y|], [|i; j|]) = let u, v = i-x, j-y in let n = max (abs u) (abs v) in [for k in [0..n] -> (x+u/n*k, y+v/n*k)]
let f, ss, q = Seq.pairwise >> Seq.collect p, System.IO.File.ReadAllLines("14"), (fun (x, y) -> [x,y+1; x-1,y+1; x+1,y+1])
let mutable bs = ss |> Seq.collect (fun s -> s.Split(" -> ") |> Seq.map (fun t -> t.Split(",") |> Array.map int) |> f) |> set
let rec g z b = bs <- Set.add b bs; q b |> Seq.filter (fun a -> snd b < z + 1 && (Set.contains a bs |> not)) |> Seq.iter (g z)
let n = bs.Count in g (bs |> Seq.map snd |> Seq.max) (500, 0); bs.Count - n
