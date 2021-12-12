let g h = List.chunkBySize 5 >> h >> List.exists (List.forall ((=) -1))

let rec f xs =
    function
    | [] -> (0, 0)
    | (y :: ys) ->
        let xs' = List.map (fun x -> if x = y then -1 else x) xs
        if g id xs' || g List.transpose xs' then
            (y * (xs' |> List.filter ((<>) -1) |> List.sum), List.length ys)
        else
            f xs' ys

let ss = System.IO.File.ReadAllLines("input.txt")

let ys =
    ss[0].Split(',')
    |> Array.map int
    |> Array.toList

let h g =
    System.Text.RegularExpressions.Regex.Split(System.String.Join(' ', Array.tail ss).Trim(), " +")
    |> Array.map int
    |> Array.toList
    |> List.chunkBySize 25
    |> List.map (fun xs -> f xs ys)
    |> g snd
    |> fst

(h List.maxBy, h List.minBy)
