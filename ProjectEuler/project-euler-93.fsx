// 2021-08-24

let flip f x y = f y x
let fs = [ (+); (-); (*); (/) ]
let gs = fs @ [ flip (-); flip (/) ]

[ for a in [ 1.0 .. 9.0 ] do
      for b in [ 1.0 .. 9.0 ] |> Seq.except [ a ] do
          for f in fs do
              let x = f a b

              for c in [ 1.0 .. 9.0 ] |> Seq.except [ a; b ] do
                  for g in gs do
                      let y = g x c

                      for d in [ 1.0 .. 9.0 ] |> Seq.except [ a; b; c ] do
                          for h in gs do
                              for z in [ h y d; h x (g c d) ] do
                                  if z > 0.0 then
                                      let z' = System.Math.Round z

                                      if System.Math.Abs(z - z') < 0.000000001 then
                                          yield ([ a; b; c; d ] |> List.sort, z') ]
|> Seq.groupBy fst
|> Seq.map (fun (xs, ys) -> (xs, ys |> Seq.map snd |> Set.ofSeq))
|> Seq.maxBy
    (fun (_, ys) ->
        seq { 1.0 .. 1000.0 }
        |> Seq.takeWhile ys.Contains
        |> Seq.length)
|> fst
|> List.map int
