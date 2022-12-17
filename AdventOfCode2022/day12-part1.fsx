let s, i0, i1, w = let s = System.IO.File.ReadAllText("12") in s, s.IndexOf('S'), s.IndexOf('E'), s.IndexOf('\n') + 1
let xs = Seq.map (function | ('\r' | '\n') -> '}' | 'S' -> 'a' | 'E' -> 'z' | c -> c) s |> Seq.map int |> Seq.toArray
let g i = [i+1; i-1; i+w; i-w] |> Seq.filter (fun j -> j >= 0 && j < s.Length && xs[j] <= xs[i] + 1)
let f (is, vs) = is |> Seq.collect g |> Set.ofSeq |> (fun js -> Set.difference js vs) |> (fun js -> Some(js, (js, Set.union vs js)))
Seq.unfold f (Set.singleton i0, Set.singleton i0) |> Seq.findIndex (Set.contains i1) |> ((+) 1)
