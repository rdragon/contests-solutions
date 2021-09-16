// 2021-08-23
// This solution was written after reading the problem forum.
// I hadn't seen the combinatorial equation mentioned there.

let target = 2_000_000
let upperBound = target * 6 / 5

let g (m, n) =
    let c = m * (m + 1) * n * (n + 1) / 4

    if c < upperBound then
        Some((c, m, n), (m, n + 1))
    else
        None

let f m =
    match List.unfold g (m, m) with
    | [] -> None
    | ts -> Some(ts, m + 1)

Seq.unfold f 1
|> Seq.concat
|> Seq.minBy (fun (c, _, _) -> System.Math.Abs(c - target))
|> (fun (_, m, n) -> m * n)
