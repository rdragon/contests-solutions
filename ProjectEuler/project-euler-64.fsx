// 2021-08-21

let rec g n a mp x y i =
    // Why does x always divide n - y * y?
    let x' = (n - y * y) / x
    let b = (a + y) / x'
    let y' = x' * b - y

    match Map.tryFind (x', y') mp with
    | Some j -> i - j
    | None -> g n a (Map.add (x', y') i mp) x' y' (i + 1)

let f n =
    let a =
        System.Math.Sqrt(float n)
        |> System.Math.Truncate
        |> int

    if a * a = n then
        0
    else
        g n a Map.empty 1 a 0

[ 1 .. 10_000 ]
|> List.map f
|> List.filter (fun x -> x &&& 1 = 1)
|> List.length
