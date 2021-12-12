let g s = if System.Char.IsLower(Seq.head s) then Set.add s else id

let rec f x vs b ps =
    if x = "end" then
        1
    else
        ps
        |> Seq.filter (fst >> ((=) x))
        |> Seq.map snd
        |> Seq.filter (fun y -> if b then not (vs |> Set.contains y) else y <> "start")
        |> Seq.sumBy (fun y -> f y (g y vs) (b || vs |> Set.contains y) ps)

let h b =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> s.Split('-'))
    |> Array.map (fun xs -> [| (xs[0], xs[1]); (xs[1], xs[0]) |])
    |> Array.concat
    |> f "start" (Set.singleton "start") b

(h true, h false)
