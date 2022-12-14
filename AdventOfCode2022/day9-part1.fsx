let f a (b, c) = if max (abs (fst c - fst a)) (abs (snd c - snd a)) > 1 then b else a
let g (x, y) = function | 'R' -> (x + 1, y) | 'U' -> (x, y + 1) | 'L' -> (x - 1, y) | _ -> (x, y - 1)
let h (s : string) = Seq.replicate (s.Substring(2) |> int) s[0]
System.IO.File.ReadAllLines("9") |> Seq.collect h |> Seq.scan g (0, 0) |> Seq.pairwise |> Seq.scan f (0, 0) |> Set.ofSeq |> Set.count
