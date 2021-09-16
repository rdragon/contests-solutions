// 2021-08-28

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

let uncurry f (x, y) = f x y

let getCell m i j = Map.tryFind (i, j) m

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

let rec solve m i j =
    if i = 9 then
        Some(getSum m)
    else
        let (i', j') = next i j

        if Option.isSome (getCell m i j) then
            solve m i' j'
        else
            let xs = getAll m i j

            let f x =
                let m' = Map.add (i, j) x m
                solve m' i' j'

            [ 1 .. 9 ]
            |> Seq.except xs
            |> Seq.choose f
            |> Seq.tryHead

let text =
    (new System.Net.Http.HttpClient()).GetStringAsync(
        "https://projecteuler.net/project/resources/p096_sudoku.txt"
    )
        .Result

let coords = iterate (uncurry next) (0, 0)

let run cs =
    let m =
        cs
        |> Seq.map (string >> int)
        |> Seq.zip coords
        |> Seq.filter (snd >> ((<) 0))
        |> Map.ofSeq

    solve m 0 0

text
|> Seq.filter System.Char.IsDigit
|> Seq.chunkBySize 83
|> Seq.sumBy (Seq.skip 2 >> run >> Option.get)
