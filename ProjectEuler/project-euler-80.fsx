// 2021-08-22

let sqrt (n: int) =
    let digits = [ 1 .. 9 ] |> List.map bigint
    let ten = bigint 10
    let hundred = bigint 100

    let rec f x n m =
        if m = 0 then
            x / bigint 10
        else
            let y =
                digits
                |> Seq.map (fun d -> x + d)
                |> Seq.takeWhile (fun y -> y * y < n)
                |> Seq.tryLast
                |> Option.defaultValue x

            f (y * ten) (n * hundred) (m - 1)

    f (bigint 0) (bigint n) 100

[ 2 .. 100 ]
|> Seq.except [ for x in 2 .. 10 do
                    x * x ]
|> Seq.sumBy (sqrt >> string >> Seq.sumBy (string >> int))
