// 2021-08-19

let rec f x i ps =
    match (x, i) with
    | (0, _) -> ps
    | _ when i * i > x -> Set.add x ps
    | _ ->
        if x % i = 0 then
            f (x / i) i (Set.add i ps)
        else
            f x (i + 1) ps

let g x = (f x 2 Set.empty).Count = 4

seq { 2 .. 1_000_000 }
|> Seq.windowed 4
|> Seq.tryFind (Seq.forall g)
