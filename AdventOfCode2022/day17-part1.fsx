let ds, conflict = System.IO.File.ReadAllText("17").TrimEnd(), fun us -> Seq.exists (fun v -> Set.contains v us)
let atWall d = Seq.map (fun u -> u % 7) >> (if d = '<' then (Seq.min >> ((=) 0)) else (Seq.max >> ((=) 6)))
let push us d vs = let ws = List.map ((+) (if d = '<' then -1 else 1)) vs in if atWall d vs || conflict us ws then vs else ws
let fall us vs = let ws = List.map ((+) -7) vs in if conflict us ws then vs else ws
let spawnAt u = function |0->[u;u+1;u+2;u+3]|1->[u+1;u+7;u+8;u+9;u+15]|2->[u;u+1;u+2;u+9;u+16]|3->[u;u+7;u+14;u+21]|4->[u;u+1;u+7;u+8]
let spawn us i = spawnAt (let u = Set.maxElement us in u - (u % 7) + 30) (i % 5)
let rec f us j vs = let ws = push us ds[j % ds.Length] vs in let xs = fall us ws in if xs = ws then (ws, j + 1) else f us (j + 1) xs
let g (us, j) i = spawn us i |> f us j |> (fun (vs, k) -> (Set.union us (vs |> set), k))
Seq.fold g ([0..6] |> set, 0) [0..2021] |> fst |> Seq.max |> (fun u -> u / 7)
