let ss = System.IO.File.ReadAllLines("input.txt")

ss
|> Array.map (Seq.map (fun c -> if c = '0' then 0 else 1))
|> Array.reduce (fun xs ys -> Seq.zip xs ys |> Seq.map (fun (x, y) -> x + y))
|> Seq.map (fun x -> if x * 2 < ss.Length then '0' else '1')
|> Seq.toArray
|> System.String
|> (fun s -> System.Convert.ToInt32(s, 2))
|> (fun x -> x * (pown 2 ss[0].Length - 1 - x))
