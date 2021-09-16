// 2021-08-18
let getBit : char -> int = string >> int >> ((<<<) 1)
let tryAddChar c m = let b = getBit c in if (c > '0') && (b &&& m = 0) then Some (b ||| m) else None
let tryAdd x m = string x |> Seq.fold (fun m' c -> Option.map (tryAddChar c) m' |> Option.flatten) (Some m)
let rec f n m xs x =
    let y = x * n
    match tryAdd y m with
    | Some 0b1111111110 -> (y :: xs) |> List.rev |> Seq.collect string |> Seq.toArray |> System.String |> int |> Some
    | Some m' -> f (n + 1) m' (y :: xs) x
    | None -> None
[1..9_999] |> List.choose (f 1 0 []) |> List.max