type U =
    | L of int
    | P of U * U

let rec addFirst x =
    function
    | L y -> L (x + y)
    | P (u, v) -> P (addFirst x u, v)

let rec addLast x =
    function
    | L y -> L (x + y)
    | P (u, v) -> P (u, addLast x v)

let rec explode d =
    function
    | L x -> (L x, None)
    | P (L x, L y) when d = 4 -> (L 0, Some (x, y))
    | P (u, v) ->
        let (u, p) = explode (d + 1) u
        match p with
        | Some (x, y) ->
            let v = addFirst y v
            (P (u, v), Some (x, 0))
        | None ->
            let (v, p) = explode (d + 1) v
            match p with
            | Some (x, y) ->
                let u = addLast x u
                (P (u, v), Some (0, y))
            | None ->
                (P (u, v), None)

let rec split =
    function
    | L x when x < 10 -> (L x, false)
    | L x -> (P (L (x / 2), L ((x + 1) / 2)), true)
    | P (u, v) ->
        let (u, b) = split u
        if b then
            (P (u, v), true)
        else
            let (v, b) = split v
            (P (u, v), b)

let rec reduce u =
    let (u, o) = explode 0 u
    if o <> None then
        reduce u
    else
        let (u, b) = split u
        if b then reduce u else u

let add u v = reduce (P (u, v))

let rec magnitude =
    function
    | L x -> x
    | P (u, v) -> 3 * magnitude u + 2 * magnitude v

let part1 = Array.reduce add >> magnitude

let part2 us =
    [for u in us do
        for v in us do
            if u <> v then add u v |> magnitude]
    |> List.max

let rec readU (xs: list<char>) =
    if System.Char.IsDigit xs[0] then
        (L (xs[0] |> string |> int), List.tail xs)
    else
        let (u, xs) = readU (List.tail xs)
        let (v, xs) = readU (List.tail xs)
        (P (u, v), List.tail xs)

System.IO.File.ReadAllLines("input.txt")
|> Array.map (Seq.toList >> readU >> fst)
|> (fun us -> (part1 us, part2 us))
