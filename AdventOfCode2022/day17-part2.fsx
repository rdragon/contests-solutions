let ds, n, conflict = System.IO.File.ReadAllText("17").TrimEnd(), 20000, fun us -> Seq.exists (fun v -> Set.contains v us)
let atWall d = Seq.map (fun u -> u % 7) >> (if d = '<' then (Seq.min >> ((=) 0)) else (Seq.max >> ((=) 6)))
let push us d vs = let ws = List.map ((+) (if d = '<' then -1 else 1)) vs in if atWall d vs || conflict us ws then vs else ws
let fall us vs = let ws = List.map ((+) -7) vs in if conflict us ws then vs else ws
let spawnAt u = function |0->[u;u+1;u+2;u+3]|1->[u+1;u+7;u+8;u+9;u+15]|2->[u;u+1;u+2;u+9;u+16]|3->[u;u+7;u+14;u+21]|4->[u;u+1;u+7;u+8]
let spawn us i = spawnAt (let u = Set.maxElement us in u - (u % 7) + 30) (i % 5)
let rec f us j vs = let ws = push us ds[j % ds.Length] vs in let xs = fall us ws in if xs = ws then (ws, j + 1) else f us (j + 1) xs
let g (us, j) i = spawn us i |> f us j |> (fun (vs, k) -> let w = Set.union us (vs |> set) in (Set.maxElement w / 7, (w, k)))
let ps = Seq.mapFold g ([0..6] |> set, 0) [0..n-1] |> fst |> Seq.map int64 |> Seq.toArray
let qs = ps |> Seq.pairwise |> Seq.map (fun (x, y) -> y - x) |> Seq.rev |> Seq.toList
let m = [5..5..5000] |> Seq.find (fun x -> let y = 10000 / x in List.replicate y (List.take x qs) |> List.collect id = List.take (x * y) qs)
let x, y = 1000000000000L - int64 (n - m), int64 m in ps[n - m - 1 + int (x - x / y * y)] + (x / y) * (ps[n - 1] - ps[n - m - 1])
