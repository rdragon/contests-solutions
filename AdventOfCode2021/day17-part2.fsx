open System.Text.RegularExpressions

let [|x1; x2; y1; y2|] =
    System.IO.File.ReadAllText("input.txt")
    |> (fun s -> Regex.Matches(s, "[0-9-]+"))
    |> Seq.map (string >> int)
    |> Seq.toArray

let rec f x y dx dy =
    if x >= x1 && x <= x2 && y >= y1 && y <= y2 then
        1
    elif dx = 0 && x < x1 || x > x2 || y < y1 then
        0
    else
        f (x + dx) (y + dy) (dx - dx / max 1 (abs dx)) (dy - 1)

[for dx in 0 .. x2 do
    for dy in y1 .. -y1 do
        f 0 0 dx dy]
|> List.sum
