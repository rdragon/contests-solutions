// 2021-08-15
let rec f = function
    | i when i < 14 -> [""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"; "ten"; "eleven"; "twelve"; "thirteen"].[i]
    | 15 -> "fifteen"
    | 18 -> "eighteen"
    | 20 -> "twenty"
    | 30 -> "thirty"
    | 40 -> "forty"
    | 50 -> "fifty"
    | 60 -> "sixty"
    | 70 -> "seventy"
    | 80 -> "eighty"
    | 90 -> "ninety"
    | 1000 -> "one thousand"
    | i when i < 20 -> let j = i % 10 in f j + "teen"
    | i when i < 100 -> let j = i % 10 in f (i - j) + " " + f j
    | i ->
        let j = i / 100
        let k = i - j * 100
        let t = if k = 0 then "" else " and " + f (i - j * 100)
        f j + " hundred" + t
[1..1000] |> List.map f |> List.sumBy (fun s -> Seq.filter ((<>) ' ') s |> Seq.length)