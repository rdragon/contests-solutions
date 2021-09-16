// 2021-08-28
// I first calculated some terms and then I saw a recurrence relation.
// However, why there exists a recurrence relation I don't know.

let threshold = 707_106_781_170L

let rec f x y =
    let z = 6L * y - x - 2L
    if y + z >= threshold then z else f y z

f 3L 15L
