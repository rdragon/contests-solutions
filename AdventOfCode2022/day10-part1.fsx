let f (s : string) = if int s[0] < 97 then int s else 0
let g = Seq.scan (+) 1 >> Seq.mapi (*) >> Seq.skip 20 >> Seq.chunkBySize 40 >> Seq.sumBy Seq.head
System.IO.File.ReadAllLines("10") |> Seq.collect (fun s -> s.Split(' ')) |> Seq.map f |> Seq.append [0] |> g
