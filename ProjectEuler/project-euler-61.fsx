// 2021-08-21

/// Generates the polygonal numbers p of type i between 1000 and 9999 as pairs (x, y) such that p = x * 100 + y.
let getNumbers i =
    seq { 1 .. 1000 }
    |> Seq.map (fun n -> n * ((i - 2) * n - i + 4) / 2)
    |> Seq.skipWhile ((>) 1000)
    |> Seq.takeWhile ((>=) 9999)
    |> Seq.map (fun x -> (x / 100, x % 100))

// a  = the first two digits of the first number in the ordered set.
// x  = the last two digits of the last added number to the ordered set.
// s  = the sum of the numbers currently in the ordered set.
// ys = the numbers that can still be added to the ordered set. These are already filtered on their polygonal type.
// n  = the amount of numbers that yet need to be added to the ordered set.
let rec find a x s ys =
    function
    | 0 -> if a = x then Some s else None
    | n ->
        ys
        |> Seq.filter (fun (_, (y, _)) -> y = x)
        |> Seq.choose (fun (i, (y, z)) -> find a z (s + y * 100 + z) (List.filter (fst >> ((<>) i)) ys) (n - 1))
        |> Seq.tryHead

[ 3 .. 8 ]
|> List.map (getNumbers >> List.ofSeq)
|> List.indexed
|> List.map (fun (i, ps) -> List.map (fun p -> (i, p)) ps)
|> List.sortBy List.length
|> (fun (ps :: qss) ->
    let qs = List.concat qss
    Seq.choose (fun (_, (x, y)) -> find x y (x * 100 + y) qs 5) ps)
|> Seq.head
