// 2021-08-28

let isSquare x =
    let y =
        System.Convert.ToInt32(System.Math.Sqrt(float x)) in y * y = x

let getMask i =
    let rec f m xs =
        function
        | [] -> xs
        | (c :: cs) ->
            match Map.tryFind c m with
            | Some x -> f m (x :: xs) cs
            | None ->
                let x = Seq.length m
                let m' = Map.add c x m
                f m' (x :: xs) cs

    f Map.empty [] (string i |> Seq.toList)

let squares =
    [ for i in 1 .. 31_623 do
          i * i ]
    |> Seq.groupBy getMask
    |> Seq.map (fun (x, ys) -> (x, Seq.sort ys |> Seq.toList))
    |> Map.ofSeq

let getMap s i =
    Seq.zip s (string i) |> Seq.distinct |> Map.ofSeq

let getI s m =
    s |> Seq.map (fun c -> Map.find c m) |> Seq.toArray |> System.String |> int

let find' s t i =
    let m = getMap s i
    let j = getI t m
    if isSquare j && Seq.length (string j) = Seq.length s then [max i j] else []

let find s t =
    match squares |> Map.tryFind (getMask s) with
    | None -> Seq.empty
    | Some is -> Seq.collect (find' s t) is

let text =
    (new System.Net.Http.HttpClient()).GetStringAsync(
        "https://projecteuler.net/project/resources/p098_words.txt"
    )
        .Result

System.Text.RegularExpressions.Regex.Matches(text, @"\b[A-Z]+\b")
|> Seq.map (fun x -> x.Value)
|> Seq.groupBy (Seq.sort >> Seq.toList)
|> Seq.map (snd >> Seq.toList)
|> Seq.filter (Seq.length >> ((<) 1))
|> Seq.collect (fun (s :: t :: _) -> find s t)
|> Seq.max
