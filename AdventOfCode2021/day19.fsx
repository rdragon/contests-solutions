let add (x, y, z) (x', y', z') = (x + x', y + y', z + z')
let sub (x, y, z) (x', y', z') = (x - x', y - y', z - z')
let man (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

let rec tryFindTranslation ps qs =
    seq {
        for p in Seq.skip 11 ps do
            for q in qs do
                let r = sub p q
                let qs = qs |> Seq.map (add r)
                let n =
                    qs
                    |> Seq.filter (fun q -> Set.contains q ps)
                    |> Seq.length
                if n >= 12 then (Set.ofSeq qs, r) }
    |> Seq.tryHead

let rotations =
    let rotX (x, y, z) = (x, z, -y)
    let rotY (x, y, z) = (z, y, -x)
    let rotZ (x, y, z) = (y, -x, z)
    let rec f i g = if i = 0 then id else (g >> f (i - 1) g)
    [ for j in 0 .. 3 do
        for i in 0 .. 3 -> f i rotX >> f j rotZ
        for i in [1; 3] -> f i rotY >> f j rotZ ]

let tryFindRotation ps qs =
    rotations
    |> Seq.map (fun f -> tryFindTranslation ps (Seq.map f qs))
    |> Seq.choose id
    |> Seq.tryHead

let rec reduce xs =
    function
    | [] -> xs
    | (qs :: ys) ->
        xs
        |> Seq.map (fun (ps, _) -> tryFindRotation ps qs)
        |> Seq.choose id
        |> Seq.tryHead
        |> (fun x ->
            match x with
            | None -> reduce xs (ys @ [qs])
            | Some x -> reduce (x :: xs) ys)

let rec read ps =
    function
    | [] -> [ps]
    | (s :: ss) ->
        if Seq.item 1 s = '-' then
            ps :: read Set.empty ss
        else
            let p = (string s).Split(',') |> Array.map int |> (fun [|x; y; z|] -> (x, y, z))
            read (Set.add p ps) ss

let part1 = Seq.map fst >> Seq.concat >> Seq.distinct >> Seq.length
let part2 = Seq.map snd >> (fun ps -> [for p in ps do for q in ps -> man p q] |> List.max)

System.IO.File.ReadAllLines("input.txt")
|> Array.tail
|> Array.filter ((<>) "")
|> Array.toList
|> read Set.empty
|> (fun (ps :: xs) -> reduce [(ps, (0, 0, 0))] xs)
|> (fun xs -> (part1 xs, part2 xs))
