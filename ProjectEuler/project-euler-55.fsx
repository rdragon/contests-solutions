// 2021-08-19

let next n =
    n
    + (n
       |> string
       |> Seq.rev
       |> Seq.toArray
       |> System.String
       |> bigint.Parse)

let isPalin n =
    let cs = string n |> Seq.toList in cs = List.rev cs

let rec isLychrel i n =
    let n' = next n

    if isPalin n' then false
    elif i = 50 then true
    else isLychrel (i + 1) n'

[ 1 .. 9_999 ]
|> List.map bigint
|> List.filter (isLychrel 2)
|> List.length
