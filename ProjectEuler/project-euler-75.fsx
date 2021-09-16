// 2021-08-22

let maxL = 1_500_000

let rec gcd x y = if y = 0 then x else gcd y (x % y)

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

seq {
    for m in 1 .. 866 do
        let maxN = min (maxL / 2 / m - m) (m - 1)
        let mIsEven = m &&& 1 = 0
        let mSquared = m * m

        for n in 1 .. maxN do
            let nIsEven = n &&& 1 = 0

            if mIsEven <> nIsEven && gcd m n = 1 then
                let l = mSquared * 2 + 2 * m * n
                yield! (iterate ((+) l) l |> Seq.takeWhile ((>=) maxL))
}
|> Seq.countBy id
|> Seq.filter (snd >> ((=) 1))
|> Seq.length
