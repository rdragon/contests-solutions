let f (p, d) (s: string) =
    let x = int (s.Split([| ' ' |])[1])
    match s[0] with
    | 'f' -> (p + x, d)
    | 'u' -> (p, d - x)
    | _ -> (p, d + x)

System.IO.File.ReadAllLines("input.txt")
|> Array.fold f (0, 0)
|> (fun (i, j) -> i * j)
