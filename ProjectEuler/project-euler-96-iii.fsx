// 2021-08-28
// Improves the performance from 5 to 0.6 seconds (on my machine) by choosing as next square to be filled a square with the least amount
// of allowed possible numbers.

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

let uncurry f (x, y) = f x y

let getIndex i j = i * 9 + j

let getCell m i j =
    match Array.get m (getIndex i j) with
    | 0 -> None
    | x -> Some x

let hasValue m i j = Option.isSome (getCell m i j)

let getRow m i =
    [ 0 .. 8 ] |> Seq.choose (fun k -> getCell m i k)

let getCol m j =
    [ 0 .. 8 ] |> Seq.choose (fun k -> getCell m k j)

let getBox m i j =
    let k = i / 3 * 3
    let l = j / 3 * 3

    Seq.allPairs [ k .. k + 2 ] [ l .. l + 2 ]
    |> Seq.choose (fun (x, y) -> getCell m x y)

let getAll m i j =
    Seq.concat [ getRow m i
                 getCol m j
                 getBox m i j ]

let getSum m =
    let [ Some x; Some y; Some z ] = [ 0 .. 2 ] |> List.map (getCell m 0)
    x * 100 + y * 10 + z

let next i j =
    let i' = i + (if j = 8 then 1 else 0)
    (i', (j + 1) % 9)

let coords =
    iterate (uncurry next) (0, 0)
    |> Seq.take 81
    |> Seq.toList

let rec solve m s =
    if s = 81 then
        Some(getSum m)
    else
        let f (i, j, p, xs) (k, l) =
            if p = 8 then
                (i, j, p, xs)
            else
                let ys = getAll m k l
                let q = ys |> Seq.distinct |> Seq.length

                if q > p then
                    (k, l, q, ys)
                else
                    (i, j, p, xs)

        let (i, j, _, xs) =
            coords
            |> Seq.filter (uncurry (hasValue m) >> not)
            |> Seq.fold f (0, 0, 0, Seq.empty)

        let f x =
            Array.set m (getIndex i j) x
            let r = solve m (s + 1)
            Array.set m (getIndex i j) 0
            r

        [ 1 .. 9 ]
        |> Seq.except xs
        |> Seq.choose f
        |> Seq.tryHead

let text =
    (new System.Net.Http.HttpClient()).GetStringAsync(
        "https://projecteuler.net/project/resources/p096_sudoku.txt"
    )
        .Result

let run cs =
    let m =
        cs |> Seq.map (string >> int) |> Array.ofSeq

    let s = m |> Seq.filter ((<) 0) |> Seq.length

    solve m s

text
|> Seq.filter System.Char.IsDigit
|> Seq.chunkBySize 83
|> Seq.sumBy (Seq.skip 2 >> run >> Option.get)
