// 2021-08-16
let rec f d carry map i =
    let n = carry |> Seq.unfold (fun j -> Some (j, j * 10)) |> Seq.find ((<=) d)
    match Map.tryFind n map with
    | Some j -> i - j
    | None ->
        let carry' = n % d
        let m = n / d
        let digits = m.ToString().Length
        let map' = Map.add n i map
        if carry' = 0 then 0 else f d carry' map' (i + digits)
Seq.maxBy (fun d -> f d 1 Map.empty 0) (seq { 2 .. 999 })