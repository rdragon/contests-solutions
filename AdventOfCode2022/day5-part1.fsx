let f = Seq.transpose >> Seq.chunkBySize 4 >> Seq.map (Array.item 1 >> Seq.skipWhile ((=) ' ') >> Seq.toList) >> Seq.toArray
let g = Seq.collect (fun (s : string) -> s.Split(' ') |> (fun x -> Seq.replicate (int x[1]) (int x[3] - 1, int x[5] - 1)))
let h (x : char list array) (s, t) = let (i::is, js) = (x[s], x[t]) in x[s] <- is; x[t] <- i::js; x
let u x = Seq.fold h x >> Seq.map Seq.head >> Seq.toArray >> System.String
System.IO.File.ReadAllLines("5") |> (fun x -> let i = Array.findIndex ((=) "") x in (f x[..i], g x[i+1..])) ||> u
