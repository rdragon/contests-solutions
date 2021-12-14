let rec f us (ss: string[]) =
    let g (x, y, n) =
        ss
        |> Seq.tryFind (fun s -> s[0] = x && s[1] = y)
        |> Option.map (fun s -> [(x, Seq.last s, n); (Seq.last s, y, n)])
        |> Option.defaultValue [(x, y, n)]
    function
    | 0 -> us
    | k ->
        us
        |> Seq.map g
        |> Seq.concat
        |> Seq.groupBy (fun (x, y, _) -> (x, y))
        |> Seq.map (fun ((x, y), vs) -> (x, y, vs |> Seq.sumBy (fun (_, _, n) -> n)))
        |> (fun vs -> f vs ss (k - 1))

let g =
    Seq.append " "
    >> Seq.pairwise
    >> Seq.map (fun (x, y) -> (x, y, 1L))

System.IO.File.ReadAllLines("input.txt")
|> (fun ss -> f (g ss[0]) (ss |> Array.skip 2) 40)
|> Seq.map (fun (_, y, n) -> (y, n))
|> Seq.groupBy fst
|> Seq.map (snd >> Seq.sumBy snd)
|> Seq.sort
|> (fun xs -> Seq.last xs - Seq.head xs)
