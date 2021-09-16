open System

let readInts () =
    Console.In.ReadLine().Split([| ' ' |])
    |> Seq.toList
    |> List.map int

let g x0 x1 x c u d =
    if c = u then
        (x0, x, (x - x0) / 2 + x0)
    elif c = d then
        (x + 1, x1, (x1 - x - 1) / 2 + x + 1)
    else
        (x0, x1, x)

let rec f x0 x1 x y0 y1 y : unit =
    let s = Console.In.ReadLine()
    let (x0', x1', x') = g x0 x1 x (Seq.last s) 'L' 'R'
    let (y0', y1', y') = g y0 y1 y s.[0] 'U' 'D'
    printfn "%i %i" x' y'
    f x0' x1' x' y0' y1' y'

let [ w; h ] = readInts ()
Console.In.ReadLine()
let [ x; y ] = readInts ()
f 0 w x 0 h y
