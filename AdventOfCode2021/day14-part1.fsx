let rec f xs (ss: string[]) =
    let g (x, y) =
        ss
        |> Seq.tryFind (fun s -> s[0] = x && s[1] = y)
        |> Option.map (fun s -> [Seq.last s; y])
        |> Option.defaultValue [y]
    function
    | 0 -> xs
    | k ->
        xs
        |> Seq.append " "
        |> Seq.pairwise
        |> Seq.map g
        |> Seq.concat
        |> (fun xs -> f xs ss (k - 1))

System.IO.File.ReadAllLines("input.txt")
|> (fun ss -> f ss[0] (ss |> Array.skip 2) 10)
|> Seq.groupBy id
|> Seq.map (snd >> Seq.length)
|> Seq.sort
|> (fun xs -> Seq.last xs - Seq.head xs)
