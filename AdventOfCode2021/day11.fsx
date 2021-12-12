let rec f xs = function
| [] -> 0
| (i :: is) ->
    if i < 0 || i >= Array.length xs then
        f xs is
    else
        Array.set xs i (xs[i] + 1)
        if xs[i] = 10 then
            1 + f xs (i - 1 :: i + 1 :: i - 12 :: i + 12 :: i - 11 :: i + 11 :: i - 13 :: i + 13 :: is)
        else
            f xs is

let rec g b xs k m =
    if b && k = 100 then
        m
    else
        let n = f xs [0 .. xs.Length]
        if n = 100 then
            k + 1
        else
            let xs' = Array.map (fun x -> if x < 10 then x else 0) xs
            g b xs' (k + 1) (m + n)

let h b =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (Seq.toList >> List.map (string >> int))
    |> Array.map (fun xs -> (-10000 :: xs @ [-10000]) |> List.toArray)
    |> Array.concat
    |> (fun xs -> g b xs 0 0)

(h true, h false)
