// 2021-08-18
let rec f s ps =
    match ps with
    | [] -> if List.head s <> '0' then seq { s |> List.toArray |> System.String |> int64 } else Seq.empty
    | (p :: ps') ->
        let g c =
            let s' = c :: s
            if Seq.truncate 3 s' |> Seq.toArray |> System.String |> int |> (fun x -> x % p = 0) then f s' ps' else Seq.empty
        "0123456789" |> Seq.except s |> Seq.collect g
f List.empty [1; 1; 17; 13; 11; 7; 5; 3; 2; 1] |> Seq.sum