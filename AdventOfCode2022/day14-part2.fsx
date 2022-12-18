let p ([|x; y|], [|i; j|]) = let u, v = i-x, j-y in let n = max (abs u) (abs v) in [for k in [0..n] -> (x+u/n*k, y+v/n*k)]
let f, q, r, ss = Seq.pairwise >> Seq.collect p, Option.map, Option.defaultValue, System.IO.File.ReadAllLines("14")
let cs = ss |> Seq.collect (fun s -> s.Split(" -> ") |> Seq.map (fun t -> t.Split(",") |> Array.map int) |> f)
let rec g z bs (x, y) = [x,y+1; x-1,y+1; x+1,y+1] |> Seq.tryFind (fun a -> y < z && (Set.contains a bs |> not)) |> q (g z bs) |> r (x, y)
let rec h z n bs = let (x, y) = g z bs (500, 0) in if y = 0 then (n + 1) else h z (n + 1) (Set.add (x, y) bs)
cs |> Set.ofSeq |> h (cs |> Seq.map snd |> Seq.max |> ((+) 1)) 0
