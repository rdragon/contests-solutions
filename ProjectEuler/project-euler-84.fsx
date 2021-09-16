// 2021-08-22
// Contains a bug: if you need to go three places back, and you land on a CC or CH square, then no additional card will be drawn.
// However, this bug doesn't influence the final result.

let steps = 10_000_000
let ar = Array.replicate 40 0
let r = System.Random()
let sides = 4

let nextR =
    function
    | 7 -> 15
    | 22 -> 25
    | 36 -> 5

let nextU =
    function
    | 7 -> 12
    | 22 -> 28
    | 36 -> 12

let rec f x dc cc ch =
    function
    | 0 -> ()
    | n ->
        let d1 = r.Next(1, sides + 1)
        let d2 = r.Next(1, sides + 1)
        let mask = if d1 = d2 then dc ||| 4 else dc
        let dc' = if mask = 7 then 0 else mask >>> 1

        let y =
            if mask = 7 then
                10
            else
                (x + d1 + d2) % 40

        let (z, cc', ch') =
            match y with
            | 2
            | 17
            | 33 ->
                let z' =
                    match cc with
                    | 1 -> 0
                    | 2 -> 10
                    | _ -> y

                (z', (cc + 1) &&& 15, ch)
            | 7
            | 22
            | 36 ->
                let z' =
                    match ch with
                    | 1 -> 0
                    | 2 -> 10
                    | 3 -> 11
                    | 4 -> 24
                    | 5 -> 39
                    | 6 -> 5
                    | 7
                    | 8 -> nextR y
                    | 9 -> nextU y
                    | 10 -> (y + 37) % 40
                    | _ -> y

                (z', cc, (ch + 1) &&& 15)
            | 30 -> (10, cc, ch)
            | _ -> (y, cc, ch)

        Array.set ar z (Array.get ar z + 1)
        f z dc' cc' ch' (n - 1)

f 0 0 0 0 steps

ar
|> Seq.map (fun x -> float x / float steps * 100.0)
|> Seq.indexed
|> Seq.sortByDescending snd
|> Seq.take 3
|> Seq.collect (fst >> string)
|> Seq.toArray
|> System.String
