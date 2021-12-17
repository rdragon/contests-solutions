let ss = System.IO.File.ReadAllLines("input.txt")
let n = Array.length ss + 2

let rec h p =
    function
    | [] -> [p]
    | (q :: qs) ->
        if fst p <= fst q then
            p :: q :: qs
        else
            q :: h p qs

let rec f (xs: int[]) vs ps =
    let ((r, i) :: qs) = ps
    if i = n * n - n - 2 then
        r
    elif i < 0 || i >= n * n || Set.contains i vs then
        f xs vs qs
    else
        let g d = h (r + xs[i + d], i + d)
        qs
        |> (g 1 >> g -1 >> g n >> g -n)
        |> f xs (Set.add i vs)

let g x xs = x :: Seq.toList xs @ [x]

ss
|> Seq.map (Seq.map (string >> int) >> Seq.toList)
|> Seq.map (g 10000)
|> g (List.replicate n 10000)
|> Seq.map List.toArray
|> Array.concat
|> (fun xs -> f xs Set.empty [(0, n + 1)])
