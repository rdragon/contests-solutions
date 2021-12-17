open System.Collections.Generic

let ss = System.IO.File.ReadAllLines("input.txt")
let n = 5 * Array.length ss + 2

let rec f (xs: int[]) vs (ps: SortedDictionary<int*int, int>) =
    if ps.Count = 0 then
        ps.Add((0, n + 1), 0)
    let (r, i) = Seq.head ps.Keys
    ps.Remove((r, i)) |> ignore
    if i = n * n - n - 2 then
        r
    elif Set.contains i vs then
        f xs vs ps
    else
        let g d =
            let j = i + d
            if j >= 0 && j < n * n && not (Set.contains j vs) then
                ps.TryAdd((r + xs[j], j), 0) |> ignore
        g 1; g -1; g n; g -n        
        f xs (Set.add i vs) ps

let g x xs = x :: Seq.toList xs @ [x]

let rec g' k xs =
    let f n = if n = 9 then 1 else n + 1
    if k = 1 then
        xs
    else
        xs @ g' (k - 1) (List.map f xs)

ss
|> Array.toList
|> List.map (Seq.map (string >> int) >> Seq.toList >> g' 5 >> g 10000)
|> List.concat
|> g' 5
|> List.singleton
|> g (List.replicate n 10000)
|> Seq.map List.toArray
|> Array.concat
|> (fun xs -> f xs Set.empty (new SortedDictionary<int*int, int>()))
