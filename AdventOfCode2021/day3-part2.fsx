let f b xs =
    xs
    |> Array.map (snd >> Array.head)
    |> Array.filter ((=) '1')
    |> Array.length
    |> (fun n -> if (n * 2 >= Array.length xs) = b then '1' else '0')

let rec g b =
    function
    | [| (s, _) |] -> System.Convert.ToInt32(s, 2)
    | xs ->
        let c = f b xs
        let ys =
            xs
            |> Array.filter (snd >> Array.head >> ((=) c))
            |> Array.map (fun (s, xs) -> (s, Array.tail xs))
        g b ys

let h b =
    System.IO.File.ReadAllLines("input.txt")
    |> Array.map (fun s -> (s, Seq.toArray s))
    |> g b

h true * h false
