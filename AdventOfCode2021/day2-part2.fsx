let f (p, a, d) (s: string) =
    let x = int (s.Split([| ' ' |])[1])
    match s[0] with
    | 'f' -> (p + x, a, d + x * a)
    | 'u' -> (p, a - x, d)
    | _ -> (p, a + x, d)

System.IO.File.ReadAllLines("input.txt")
|> Array.fold f (0, 0, 0)
|> (fun (i, _, j) -> i * j)
