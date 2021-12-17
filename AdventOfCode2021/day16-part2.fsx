open System

let g' = List.toArray >> String >> (fun s -> (Convert.ToInt64(s, 2)))

let g n xs =
    let (ys, zs) = List.splitAt n xs
    (g' ys, zs)

let rec h ys (x :: xs) =
    let (zs, xs) = List.splitAt 4 xs
    if x = '0' then
        (g' (ys @ zs), xs)
    else
        h (ys @ zs) xs

let rec f k xs =
    if k = 0 || List.isEmpty xs then
        ([], xs)
    else
        let (_, xs) = g 3 xs
        let (t, xs) = g 3 xs
        let (r, xs) = 
            match t with
            | 4L -> h [] xs
            | _ ->
                let (i, xs) = g 1 xs
                let (rs, xs) =
                    if i = 0 then
                        let (n, xs) = g 15 xs
                        let (ys, xs) = List.splitAt (int n) xs
                        (f 10000 ys |> fst, xs)
                    else
                        let (k, xs) = g 11 xs
                        f (int k) xs
                let r =
                    match t with
                    | 0L -> List.sum rs
                    | 1L -> List.reduce (*) rs
                    | 2L -> List.min rs
                    | 3L -> List.max rs
                    | 5L -> if rs[0] > rs[1] then 1L else 0L
                    | 6L -> if rs[0] < rs[1] then 1L else 0L
                    | _ -> if rs[0] = rs[1] then 1L else 0L
                (r, xs)

        let (rs, xs) = f (k - 1) xs
        (r :: rs, xs)

IO.File.ReadAllText("input.txt").Trim()
|> System.Convert.FromHexString
|> Array.map (fun b -> Convert.ToString(b, 2).PadLeft(8, '0'))
|> Seq.concat
|> Seq.toList
|> f 1
