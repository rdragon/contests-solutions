open System

Console.In.ReadLine()

let ans =
    match Console.In.ReadLine() with
    | "" -> 0
    | s ->
        s.Split([| ' ' |])
        |> Seq.toList
        |> List.map int
        |> Seq.minBy (fun x -> (Math.Abs x, -x))

printfn "%i" ans
