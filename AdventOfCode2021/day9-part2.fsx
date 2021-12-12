let ss =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> "9" + s + "9")
    
let n = ss[0].Length

let xs =
    ss
    |> Array.map Array.ofSeq
    |> Array.concat

let rec f m =
    function
    | [] -> m
    | (i :: is) ->
        match (if i > 0 && i < xs.Length then xs[i] else '9') with
        | '9' -> f m is
        | _ ->
            Array.set xs i '9'
            f (m + 1) (i - 1 :: i + 1 :: i - n :: i + n :: is)

[0 .. xs.Length]
|> List.map (fun i -> f 0 [i])
|> List.sortDescending
|> List.take 3
|> List.reduce (*)
