let o = Seq.mapi (fun i s -> Seq.mapi (fun j c -> if c = '#' then [i,j] else []) s |> Seq.collect id) >> Seq.collect id >> Set.ofSeq
let mutable a, xs, r = System.IO.File.ReadAllLines("23") |> o, [0..3], fun is -> Seq.min is, Seq.max is
let f (i,j) = function | 0->[i-1,j;i-1,j-1;i-1,j+1] | 1->[i+1,j;i+1,j-1;i+1,j+1] | 2->[i,j-1;i-1,j-1;i+1,j-1] | _->[i,j+1;i-1,j+1;i+1,j+1]
let h u x = if List.exists a.Contains (f u x) then None else Some (List.head (f u x))
let g u = let us = List.choose (h u) xs in if List.length us = 4 || us.IsEmpty then None else Some (List.head us)
let p _ = let b = Seq.map (fun u -> (u, g u)) a |> List.ofSeq
          let c = Seq.choose snd b |> Seq.groupBy id |> Seq.filter (snd >> Seq.length >> ((=) 1)) |> Seq.map fst |> Set.ofSeq
          let q = function | (_, Some u) when c.Contains(u) -> u | (u, _) -> u
          a <- Seq.map q b |> Set.ofSeq; xs <- List.tail xs @ [List.head xs] in Seq.iter p [0..9]
let (i,j),(k,l) = r (Seq.map fst a), r (Seq.map snd a) in Seq.allPairs [i..j] [k..l] |> Seq.filter (a.Contains >> not) |> Seq.length
