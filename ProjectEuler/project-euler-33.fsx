// 2021-08-17
// Note that the digit to remove (or add, as this implementation is adding digits instead of removing digits) cannot be both times at the
// same side.
let f x y = int (string x + string y)
let g h =
    [for x in [1..9] do
        for y in [1..9] do
            for z in [1..9] do
                if x <> z && x * f y z = f z x * y then yield h (f y z) (f z x)] |> List.reduce (*)
let rec gcd x y = if y = 0 then x else gcd y (x % y)
let numerator = g min
let denominator = g max
denominator / gcd numerator denominator