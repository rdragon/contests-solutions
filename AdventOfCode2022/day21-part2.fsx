let t, d = "humn", System.IO.File.ReadAllLines("21") |> Seq.map (fun s -> (s[0..3], s[6..].Split(' ') |> List.ofSeq)) |> Map.ofSeq
let g, q = (function | "+" -> (+) | "-" -> (-) | "*" -> (*) | _ -> (/)), fun i -> Map.keys d |> Seq.find (fun k -> List.contains i d[k])
let rec f i = match d[i] with | [s] -> int64 s | [j;c;k] -> g c (f j) (f k)
let h a b = function | "+", _ -> b-a | "*", _ -> b/a | "-", true -> a+b | "-", _ -> a-b | "/", true -> a*b | "/", _ -> a/b
let rec p i = let j = q i in let [u;c;v] = d[j] in let a = f (if u=i then v else u) in if j="root" then a else h a (p j) (c, u=i) in p t
