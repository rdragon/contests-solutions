let f (x,y) (i,j) = if i <= y && j >= x then Some ((i,j), (min x i, max y j)) else None
let rec h bs c = match Seq.choose (f c) bs |> Seq.toList with | [] -> Set.add c bs | (b, d)::_ -> h (Set.remove b bs) d
System.IO.File.ReadAllLines("15") |> Seq.map (fun s -> let xs = s.Split([|',';':';'=';' '|]) in [for i in [3;6;13;16] -> int xs[i]])
|> Seq.map (fun [x;y; i;j] -> let n = abs (x - i) + abs (y - j) - abs (y - 2_000_000) in (x-n, x+n)) |> Seq.filter (fun (x, y) -> y >= x)
|> Seq.fold h Set.empty |> Seq.sumBy (fun (x, y) -> y - x + 1) |> (fun x -> x - 1)
