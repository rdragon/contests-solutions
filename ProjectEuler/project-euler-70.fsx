// 2021-08-22

let primesToTry =
    let rec f ps x =
        let ps' =
            if List.forall (fun p -> x % p <> 0) ps then
                (x :: ps)
            else
                ps

        if x > 10_000 then
            List.rev ps'
        else
            f ps' (x + 2)

    f [ 2 ] 3 |> List.skipWhile ((>) 1_000)

let phi p q = (p - 1) * (q - 1)

let isPerm x y =
    let x' = x |> string |> Seq.sort |> Seq.toList
    let y' = y |> string |> Seq.sort |> Seq.toList
    x' = y'

[ for p in primesToTry do
      for q in
          primesToTry
          |> Seq.skipWhile ((>=) p)
          |> Seq.takeWhile (fun q -> p * q < 10_000_000) do
          if isPerm (p * q) (phi p q) then
              yield (p, q) ]
|> List.maxBy (fun (p, q) -> (1.0 - 1.0 / float p) * (1.0 - 1.0 / float q))
|> (fun (p, q) -> p * q)
