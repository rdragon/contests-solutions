System.IO.File.ReadAllLines("15") |> Seq.map (fun s -> let xs = s.Split([|',';':';'=';' '|]) in [for i in [3;6;13;16] -> int xs[i]])
|> Seq.collect (fun [x;y;i;j] -> let n = abs (x-i) + abs (y-j) - abs (y-2_000_000) in [x-n..1..x+n]) |> set |> Seq.length |> (fun x -> x-1)
