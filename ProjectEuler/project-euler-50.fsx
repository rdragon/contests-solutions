// 2021-08-19

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

let isPrime x =
    if x > 1 then
        Array.get sieve x
    else
        false

let primes = [ 1 .. maxP ] |> List.filter isPrime

let tails xs =
    let rec h xss =
        function
        | [] -> xss
        | xs' -> h (xs' :: xss) (List.tail xs')

    h [] xs |> List.rev

let f best =
    Seq.scan (+) 0
    >> Seq.takeWhile ((>) maxP)
    >> Seq.indexed
    >> Seq.skipWhile (fst >> ((>=) (fst best)))
    >> Seq.filter (snd >> isPrime)
    >> Seq.sortDescending
    >> Seq.tryHead
    >> Option.defaultValue best

tails primes |> List.fold f (0, 0) |> snd
