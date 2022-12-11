let f = Seq.transpose >> Seq.chunkBySize 4 >> Seq.map (Array.item 1 >> Seq.skipWhile ((=) ' ') >> Seq.toList) >> Seq.toArray
let g = Seq.map (fun (s : string) -> s.Split(' ') |> (fun x -> (int x[1], int x[3] - 1, int x[5] - 1)))
let h (x : char list array) (n, s, t) = let ((is, js), ks) = (List.splitAt n x[s], x[t]) in x[s] <- js; x[t] <- is @ ks; x
let u x = Seq.fold h x >> Seq.map Seq.head >> Seq.toArray >> System.String
System.IO.File.ReadAllLines("5") |> (fun x -> let i = Array.findIndex ((=) "") x in (f x[..i], g x[i+1..])) ||> u
