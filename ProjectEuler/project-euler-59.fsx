// 2021-08-21
// Could be optimized further by replacing the HashSet by a boolean two-dimensional array of size 1229 x 1229.

let maxP = 10_000

let primes =
    let rec f ps x =
        let ps' =
            if List.forall (fun p -> x % p <> 0) ps then
                (x :: ps)
            else
                ps

        if x > maxP then
            List.rev ps'
        else
            f ps' (x + 2)

    f [ 2 ] 3

let isOdd x = x &&& 1L = 1L

let rec iterate f x =
    seq {
        yield x
        yield! iterate f (f x)
    }

let modPow x n m =
    if n = 0L then
        1L
    else
        let rec f x n m r =
            if n = 1L then
                (x * r % m)
            elif isOdd n then
                f (x * x % m) ((n - 1L) >>> 1) m (x * r % m)
            else
                f (x * x % m) (n >>> 1) m r

        f x n m 1L

let millerRabin =
    function
    | n' when n' > 2 ->
        let rec f s d p =
            if isOdd d then
                (s, d)
            else
                f (s + 1) (d >>> 1) (p <<< 1)

        let n = int64 n'
        let (s, d) = f 1 ((n - 1L) >>> 1) 2L

        let check a =
            if a >= n then
                true
            else
                let x = modPow a d n

                if x = 1L || x = n - 1L then
                    true
                else
                    iterate (fun x -> x * x % n) (x * x % n)
                    |> Seq.take (s - 1)
                    |> Seq.contains (n - 1L)

        check 2L && check 7L && check 61L
    | n -> n = 2

let tails xs =
    let rec h xss =
        function
        | [] -> xss
        | xs' -> h (xs' :: xss) (List.tail xs')

    h [] xs |> List.rev

let pairs =
    let f p q =
        let (x, y) = (string p, string q)

        millerRabin (int (x + y))
        && millerRabin (int (y + x))

    [ for ps in tails primes.Tail do
          match ps with
          | (p :: qs) ->
              for q in qs do
                  if f p q then yield (p, q)
          | _ -> () ]
    |> System.Collections.Generic.HashSet

let isPair p q = pairs.Contains(min p q, max p q)

let rec findPairSets xs pss n =
    [ match pss with
      | [] -> ()
      | ((p :: _) :: qss) ->
          let qss' =
              qss
              |> List.filter (
                  List.tryHead
                  >> Option.map (isPair p)
                  >> Option.defaultValue false
              )

          if n = 1 then
              yield p :: xs
          else
              yield! findPairSets (p :: xs) qss' (n - 1)

          yield! findPairSets xs qss n ]

findPairSets [] (tails primes.Tail) 5
|> Seq.map Seq.sum
|> Seq.min
