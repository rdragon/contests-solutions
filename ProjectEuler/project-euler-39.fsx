// 2021-08-18
let maxP = 1000
let dict = Map.ofList [for i in 1..maxP do (i * i, i)]
[for a in 1..maxP do
    for b in a..(maxP - a) do
        match Map.tryFind (a * a + b * b) dict with
        | Some c when c >= b && a + b + c <= maxP -> yield a + b + c
        | _ -> () ] |> List.groupBy id |> List.maxBy (snd >> List.length)