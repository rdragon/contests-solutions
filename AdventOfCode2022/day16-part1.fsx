// Runs below 0.1 seconds on my machine.
// See part2 for some explanations.

let graph, uStart =
    let lines = System.IO.File.ReadAllLines("16")
    let uMap = lines |> Seq.sortBy (fun s -> s[23] = '0') |> Seq.mapi (fun i s -> (s[6..7], i)) |> Map.ofSeq
    lines |> Seq.map (fun s -> let u = uMap[s[6..7]] in (u, (u, s[23..24].TrimEnd(';') |> int) :: (s[49..].Split(',')
    |> Seq.map (fun t -> (uMap[t.Trim()], 0)) |> Seq.toList))) |> Map.ofSeq, uMap["AA"]

let mem = System.Collections.Generic.Dictionary<int*int, (int*int) list>()
let mutable best = 0

// The value on index `m` gives an upper bound on the score that is possible if you have `m` minutes left.
let upperBounds =
    let f m = Seq.map snd >> Seq.sortDescending >> Seq.zip [m-1..-2..1] >> Seq.sumBy (fun (x, y) -> x * y)
    [|0..30|] |> Array.map (fun m -> graph |> Map.values |> Seq.collect id |> f m)

let rec walk u m a p =
    let step (v, q) =
        let release = q > 0 && (((a >>> v) &&& 1) = 0)
        let b = if release then a ||| (1 <<< v) else a
        let r = p + (if release then (m - 1) * q else 0)
        best <- max best r
        walk v (m - 1) b r
    let list = match mem.TryGetValue((u, a)) with | (true, xs) -> xs | _ -> []
    match (m, list |> Seq.tryFind (fst >> ((<=) m)) |> Option.map snd) with
    | (0, _) -> ()
    | (_, Some q) when p <= q -> ()
    | _ when p + upperBounds[m] <= best -> ()
    | _ ->
        mem[(u, a)] <- list |> List.filter (fun (n, q) -> n > m || q > p) |> (fun xs -> (m, p) :: xs) |> List.sortBy fst
        graph[u] |> Seq.iter step

walk uStart 30 0 0; best
