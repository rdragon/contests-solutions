type V = | I of int | L of V list
let r i ts = List.map fst ts |> L, 1 + if ts = [] then i else Seq.last ts |> snd
let g f s i = List.unfold (f s >> Option.map (fun (v, j) -> (v, j), j)) (i + 1) |> r (i + 1) |> Some
let p (s : string) i = let j = s.IndexOfAny([|']'; ','|], i) in Some (s[i..j-1] |> int |> I, j)
let rec f (s : string) i = match s[i] with | '[' -> g f s i | ']' -> None | ',' -> f s (i + 1) | _ -> p s i
let rec h = function | I x, I y -> (if x = y then None else Some (x < y)) | I x, L y -> h (L [I x], L y)
                     | L x, I y -> h (L x, L [I y]) | (L [], L []) -> None | L [], _ -> Some true
                     | _, L [] -> Some false | L (x::xs), L (y::ys) -> h (x, y) |> Option.orElse (h (L xs, L ys))
let q s = f s 0 |> Option.get |> fst in System.IO.File.ReadAllLines("13") |> Seq.chunkBySize 3
|> Seq.mapi (fun i xs -> if h (q xs[0], q xs[1]) = Some true then i + 1 else 0) |> Seq.sum
