// 2021-08-19

let getScore cs =
    let (_, s1) = List.head cs
    let (vs, ss) = List.unzip cs
    let isFlush = List.forall ((=) s1) ss

    let groups =
        List.groupBy fst cs
        |> List.map snd
        |> List.sortByDescending (fun xs -> (xs.Length, fst xs.Head))

    let (group1 :: group2 :: _) = groups
    let vGroup1 = fst group1.Head

    let isStraight =
        group1.Length = 1 && List.max vs - List.min vs = 4

    let rankScore =
        match (group1.Length, group2.Length) with
        | _ when isFlush && isStraight -> (9, 0)
        | (4, _) -> (8, vGroup1)
        | (3, 2) -> (7, vGroup1)
        | _ when isFlush -> (6, 0)
        | _ when isStraight -> (5, 0)
        | (3, _) -> (4, vGroup1)
        | (2, 2) -> (3, vGroup1)
        | (2, _) -> (2, vGroup1)
        | _ -> (1, 0)

    (rankScore, List.sortDescending vs)

let getCard text =
    match Seq.toList text with
    | [ v; s ] ->
        match Seq.tryFindIndex ((=) v) "TJQKA" with
        | Some i -> (int '9' + 1 + i, s)
        | None -> (int v, s)

let text =
    (new System.Net.Http.HttpClient()).GetStringAsync(
        "https://projecteuler.net/project/resources/p054_poker.txt"
    )
        .Result

System.Text.RegularExpressions.Regex.Matches(text, "[2-9TJQKA][CDHS]")
|> Seq.map (fun x -> getCard x.Value)
|> Seq.chunkBySize 5
|> Seq.map (Array.toList >> getScore)
|> Seq.chunkBySize 2
|> Seq.filter (fun ar -> ar.[0] > ar.[1])
|> Seq.length
