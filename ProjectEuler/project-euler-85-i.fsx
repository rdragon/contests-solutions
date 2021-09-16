// 2021-08-23
// This solution computes the number of rectangles inside an m x n grid by the formula:
//    C(m, n) = 2 * C(m, n - 1) + C(m, 1) - C(m, n - 2),
// which is derived from splitting the m x n grid in two grids: m x (n - 1) and m x 1.

let target = 2_000_000
let upperBound = target * 6 / 5

let simpleCount m n =
    match (m, n) with
    | (0, _)
    | (_, 0) -> Some 0
    | (1, 1) -> Some 1
    | _ -> None

let compute cs m n =
    let count m n =
        match simpleCount m n with
        | (Some x) -> Some x
        | None -> Map.tryFind (min m n, max m n) cs

    let f m n =
        match (count m (n - 1), count m 1, count m (n - 2)) with
        | (Some x, Some y, Some z) -> Some(2 * x + y - z)
        | _ -> None
        |> Option.filter ((>) upperBound)
        |> Option.map (fun c -> (c, Map.add (m, n) c cs))

    f (min m n) (max m n)

let ans =
    let g (cs, m, n) =
        match compute cs m n with
        | Some (c, cs') -> Some((c, cs', m, n), (cs', m, n + 1))
        | None -> None

    let f (cs, m) =
        match List.unfold g (cs, m, max m 2) with
        | [] -> None
        | ts ->
            let (_, cs', _, _) = List.last ts
            Some(ts, (cs', m + 1))

    Seq.unfold f (Map.empty, 1)
    |> Seq.concat
    |> Seq.minBy (fun (c, _, _, _) -> System.Math.Abs(c - target))
    |> (fun (_, _, m, n) -> m * n)

ans
