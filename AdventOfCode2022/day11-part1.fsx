let sss = System.IO.File.ReadAllLines("11") |> Array.chunkBySize 7 |> Array.map (Array.skip 1 >> Array.take 5)
let mutable cs, xss = [], sss |> Array.map (Array.head >> (fun s -> s.Substring(18).Split(", ") |> Seq.map int |> Seq.toList))
let h x (s : string) = let [|y; z|] = s.Replace("old", string x).Split(' ') |> Array.rev |> Array.take 2 in (int y, if z = "+" then (+) else (*))
let f ss x = let xs, (_, u) = Array.map (h x >> fst) ss, h x ss[0] in let y = (u x xs[0]) / 3 in (y, if y % xs[1] = 0 then xs[2] else xs[3])
let g i = Seq.iter (fun x -> let (y, j) = f (sss[i][1..]) x in cs <- i::cs; xss[j] <- xss[j] @ [y]) xss[i]; xss[i] <- []
Seq.replicate 20 [0..sss.Length-1] |> Seq.collect id |> Seq.iter g
Seq.countBy id cs |> Seq.map snd |> Seq.sortDescending |> Seq.take 2 |> Seq.reduce (*)
