// 2021-08-17
let maxP = 1_000_000
let sieve =
    let array = Array.replicate (maxP + 1) true
    let h p =
        if Array.get array p then
            for q in p * p .. p .. maxP do
                Array.set array q false
    seq { 2 .. maxP }
        |> Seq.takeWhile (fun p -> p * p <= maxP)
        |> Seq.iter h
    array
let isPrime x = if x > 1 then Array.get sieve x else false
let show = List.toArray >> System.String
let rotations x =
    let s = string x |> Seq.toList
    let n = s.Length
    List.windowed n (s @ s) |> List.take n |> List.map (show >> int)
let goodDigits = "1379" |> Seq.toList |> set
let hasOnlyGoodDigits = string >> Seq.toList >> List.forall goodDigits.Contains
[1..maxP] |> List.filter hasOnlyGoodDigits |> List.filter (rotations >> List.forall isPrime) |> List.length |> ((+) 2)