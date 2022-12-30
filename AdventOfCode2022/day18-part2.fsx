let ws = System.IO.File.ReadAllLines("18") |> Array.map (fun s -> s.Split(',') |> Array.map int) |> set
let mutable vs, h = Set.empty, fun u -> Seq.max u > 20 || Seq.min u < -1
let g [|a;b;c|] = [[|a-1;b;c;|];[|a;b-1;c;|];[|a;b;c-1;|];[|a+1;b;c;|];[|a;b+1;c;|];[|a;b;c+1;|]]
let rec f = function | [] -> () | (u::us) -> if vs.Contains(u) || h u || ws.Contains(u) then f us else vs <- Set.add u vs; f (g u @ us)
let p _ = Set.union ws ([for a in [0..19] do for b in [0..19] do for c in [0..19] -> [|a;b;c|]] |> List.filter (vs.Contains >> not) |> set)
f [[|-1;-1;-1|]]; let xs = p () in 6 * (Seq.length xs) - 2 * (Seq.collect (g >> Seq.take 3 >> Seq.filter xs.Contains) xs |> Seq.length)
