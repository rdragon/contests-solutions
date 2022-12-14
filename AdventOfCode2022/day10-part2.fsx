let f (s : string) = if int s[0] < 97 then int s else 0
let g = Seq.mapi (fun i x -> if abs (i % 40 - x) < 2 then '#' else '.') >> Seq.chunkBySize 40 >> Seq.map (Seq.toArray >> System.String)
System.IO.File.ReadAllLines("10") |> Seq.collect (fun s -> s.Split(' ')) |> Seq.map f |> Seq.scan (+) 1 |> g |> Seq.iter (printfn "%s")
