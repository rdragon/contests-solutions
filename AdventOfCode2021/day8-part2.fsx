let f (s : string) =
    let (xs, ys) =
        s.Split(' ')
        |> Array.map Set.ofSeq
        |> Array.splitAt 11
    let g n y =
        xs
        |> Array.find (Set.count >> ((=) n))
        |> Set.intersect y
        |> Set.count
    let h y =
        match (Set.count y, g 2 y, g 4 y) with
        | (2, _, _) -> 1
        | (3, _, _) -> 7
        | (4, _, _) -> 4
        | (5, 2, _) -> 3
        | (5, _, 3) -> 5
        | (5, _, _) -> 2
        | (6, _, 4) -> 9
        | (6, 2, _) -> 0
        | (6, _, _) -> 6
        | (_, _, _) -> 8
    ys
    |> Array.map h
    |> Array.reduce (fun x y -> x * 10 + y)

System.IO.File.ReadAllLines("input.txt")
|> Array.sumBy f
