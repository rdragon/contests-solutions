// 2021-08-19

let maxP = 10_000

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

let isPrime x =
    if x > 1 then
        Array.get sieve x
    else
        false

let f = string >> Seq.sort >> Seq.toList

[ for p in [ 1000 .. 9999 ] |> Seq.filter isPrime do
      for d in [ 1 .. (9999 - p) / 2 ] do
          if isPrime (p + d)
             && isPrime (p + d + d)
             && f p = f (p + d)
             && f p = f (p + d + d) then
              yield (string p + string (p + d) + string (p + d + d))
          else
              () ]
|> List.last
