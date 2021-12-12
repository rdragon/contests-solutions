let g =
    function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | c -> c

let rec f =
    function
    | [] -> Ok []
    | (x :: xs) ->
        if g x = x then
            Ok (x :: xs)
        else
            match f xs with
            | Error c -> Error c
            | Ok [] -> Ok []
            | Ok (y :: ys) ->
                if g x = y then f ys else Error y

let h s =
    match s |> Seq.toList |> f with
    | Error ')' -> 3
    | Error ']' -> 57
    | Error '}' -> 1197
    | Error '>' -> 25137
    | _ -> 0

System.IO.File.ReadAllLines("input.txt")
|> Array.sumBy h
