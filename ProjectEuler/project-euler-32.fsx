// 2021-08-17
// Using pen and paper you can see that the number of digits of each product should equal four.
// Therefore, the multiplicand can be assumed to have one or two digits, and the multiplier
// can be assumed to have three or four digits.
let loadMask : int -> int = string >> Seq.map (string >> int >> pown 2) >> Seq.reduce (|||)
let tryLoadMask x = let s = string x in if s.Contains('0') || (set s).Count < s.Length then None else Some (loadMask x)
let masks = [0..1_000_000] |> List.map tryLoadMask |> List.toArray
let tryGetMask = Array.get masks
let f x y = x &&& y = 0
let g x y z = f x z && f y z && x ||| y ||| z = 0b1111111110
let tryAddMask x = tryGetMask x |> Option.map (fun x' -> (x, x'))
let multiplicands = [1..99] |> List.choose tryAddMask
let multipliers = [100..9_999] |> List.choose tryAddMask
[for (x, x') in multiplicands do
    for (y, y') in multipliers do
        if f x' y' then
            let z = x * y
            match tryGetMask z with
            | Some z' -> if g x' y' z' then yield z
            | None -> ()] |> List.distinct |> List.sum