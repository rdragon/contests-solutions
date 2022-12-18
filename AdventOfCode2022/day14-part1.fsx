let p ([|x; y|], [|i; j|]) = let u, v = i-x, j-y in let n = max (abs u) (abs v) in [for k in [0..n] -> (x+u/n*k, y+v/n*k)]
let f, q, r, ss = Seq.pairwise >> Seq.collect p, Option.map, Option.defaultValue, System.IO.File.ReadAllLines("14")
let rec g bs (x, y) = [x,y+1; x-1,y+1; x+1,y+1] |> Seq.tryFind (fun a -> y < 999 && (Set.contains a bs |> not)) |> q (g bs) |> r (x, y)
let rec h n bs = let (x, y) = g bs (500, 0) in if y = 999 then n else h (n + 1) (Set.add (x, y) bs)
ss |> Seq.collect (fun s -> s.Split(" -> ") |> Seq.map (fun t -> t.Split(",") |> Array.map int) |> f) |> Set.ofSeq |> h 0
