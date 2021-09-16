// 2021-08-22

let getAs n =
    let cf n a (p, q) =
        let b = (n - p * p) / q
        let c = (a + p) / b
        Some(c, (c * b - p, b))

    let a =
        n
        |> float
        |> System.Math.Sqrt
        |> System.Math.Truncate
        |> bigint

    seq {
        yield a
        yield! (Seq.unfold (cf n a) (a, bigint 1))
    }

// From https://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions_and_convergents.
let getApproxs n =
    let as' = getAs n
    let (a0 :: a1 :: _) = Seq.toList (Seq.take 2 as')
    let t0 = (a0, bigint 1)
    let t1 = (a0 * a1 + bigint 1, a1)

    let f ((h0, k0), (h1, k1), as') =
        let a2 = Seq.head as'
        let h2 = h1 * a2 + h0
        let k2 = k1 * a2 + k0
        Some((h2, k2), ((h1, k1), (h2, k2), Seq.tail as'))

    seq {
        yield t0
        yield t1
        yield! (Seq.unfold f (t0, t1, Seq.skip 2 as'))
    }

let findX d =
    getApproxs d
    |> Seq.find (fun (x, y) -> x ** 2 - d * (y ** 2) = bigint 1)
    |> fst

[ 3 .. 1000 ]
|> Seq.except [ for i in 2 .. 34 do
                    i * i ]
|> Seq.map (fun i -> (i, findX (bigint i)))
|> Seq.maxBy snd
|> fst
