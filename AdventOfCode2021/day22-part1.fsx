open System.Text.RegularExpressions

let rec f x y z =
    function
    | [] -> false
    | ((b, (x1, x2, y1, y2, z1, z2)) :: xs) ->
        if x1 <= x && x <= x2 && y1 <= y && y <= y2 && z1 <= z && z <= z2 then
            b
        else
            f x y z xs

let read s =
    Regex.Matches(s, "[0-9-]+")
    |> Seq.map (string >> int)
    |> Seq.toArray
    |> fun xs -> (xs[0], xs[1], xs[2], xs[3], xs[4], xs[5])

System.IO.File.ReadAllLines("input.txt")
|> Array.take 20
|> Array.map (fun s -> (s[1] = 'n', read s))
|> Array.rev
|> Array.toList
|> fun xs ->
    [ for x in -50 .. 50 do
        for y in -50 .. 50 do
            for z in -50 .. 50 do
                if f x y z xs then 1 ]
    |> List.sum
