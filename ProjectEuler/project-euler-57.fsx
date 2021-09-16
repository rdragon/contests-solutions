// 2021-08-20
// It's easy to see that all generated fractions are reduced, so no GCD taking is required.

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

iterate (fun (x, y) -> (x + y + y, x + y)) (bigint 3, bigint 2)
|> Seq.take 1000
|> Seq.filter (fun (x, y) -> (string x).Length > (string y).Length)
|> Seq.length
