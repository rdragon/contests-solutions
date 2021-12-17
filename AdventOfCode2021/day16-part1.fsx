open System

let g n xs =
    let (ys, zs) = List.splitAt n xs
    ys
    |> List.toArray
    |> String
    |> (fun s -> (Convert.ToInt32(s, 2), zs))

let rec h xs =
    let (y, ys) = (List.head xs, List.skip 5 xs)
    if y = '0' then ys else h ys

let rec f r xs =
    if List.forall ((=) '0') xs then
        r
    else
        let (v, xs) = g 3 xs
        let (t, xs) = g 3 xs
        let (w, xs) =
            match t with
            | 4 -> (0, h xs)
            | _ ->
                let (i, xs) = g 1 xs
                (f 0 (List.skip (15 - i * 4) xs), [])
        f (r + v + w) xs

IO.File.ReadAllText("input.txt").Trim()
|> Convert.FromHexString
|> Array.map (fun b -> Convert.ToString(b, 2).PadLeft(8, '0'))
|> Seq.concat
|> Seq.toList
|> f 0
