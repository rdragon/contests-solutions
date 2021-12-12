let rec f n xs =
    if n = 0 then
        Array.sum xs
    else
        f (n - 1) [| xs[1]; xs[2]; xs[3]; xs[4]; xs[5]; xs[6]; xs[7] + xs[0]; xs[8]; xs[0] |]

let g n =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.head
    |> (fun s -> s.Split(','))
    |> Array.map int64
    |> (fun xs -> Array.init 9 (fun i -> xs |> Seq.filter ((=) i) |> Seq.length |> int64))
    |> f n

(g 80, g 256)
