System.IO.File.ReadAllText("input.txt").Split(',')
|> Array.map int
|> Array.toList
|> List.sort
|> (fun xs ->
    let y = List.item (List.length xs / 2) xs
    List.sumBy (fun x -> abs (x - y)) xs)
