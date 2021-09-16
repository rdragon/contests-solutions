// 2021-08-19
// Removed the triangle check after reading the problem forum.

let isPent x =
    let y =
        (System.Math.Sqrt(1.0 + 24.0 * float x) + 1.0)
        / 6.0 in y = System.Math.Truncate(y)

[ 1 .. 100_000 ]
|> Seq.map (fun x -> 2 * x * x - x)
|> Seq.filter isPent
|> Seq.skip 2
|> Seq.tryHead
