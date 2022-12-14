let r f (x, y) (z, w) = (f x z, f y w)
let q a b = r (-) a b |> (fun (x, y) -> (x = 0 || y = 0))
let g a b c = if q c a then r (+) a c |> (fun (x, y) -> (x/2, y/2)) else if q c b then b else r (-) c b |> r (+) a
let f a (b, c) = if r (-) a c |> (fun (x, y) -> max (abs x) (abs y)) > 1 then g a b c else a
let h (s : string) = Seq.replicate (s.Substring(2) |> int) (match s[0] with | 'R' -> (1,0) | 'U' -> (0,1) | 'L' -> (-1,0) | _ -> (0,-1))
let rec p n = if n = 0 then id else p (n - 1) >> Seq.pairwise >> Seq.scan f (0, 0)
System.IO.File.ReadAllLines("9") |> Seq.collect h |> Seq.scan (r (+)) (0, 0) |> p 9 |> Set.ofSeq |> Set.count
