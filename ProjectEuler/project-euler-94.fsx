// 2021-08-28
// Heron's formula has been used in this solution.

let isSquare x =
    let y =
        System.Convert.ToInt64(System.Math.Sqrt(float x)) in y * y = x

let getPerimiter' a x =
    let y = x + (a <<< 2)

    if y &&& 3L = 0L then
        if isSquare (y >>> 2) then
            Some(3L * a - 1L)
        else
            None
    else
        None

let getPerimiter a =
    let x = 3L * a * a - (a <<< 1) - 1L

    if x &&& 3L = 0L then
        if isSquare (x >>> 2) then
            Some(3L * a + 1L)
        else
            getPerimiter' a x
    else
        getPerimiter' a x

seq { 3L .. 2L .. 333_333_334L }
|> Seq.choose getPerimiter
|> Seq.sum
