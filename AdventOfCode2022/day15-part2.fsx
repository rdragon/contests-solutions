let f (x,y) (i,j) = if i <= y + 1L && j >= x - 1L then Some ((i,j), (min x i, max y j)) else None
let rec h bs c = match Seq.choose (f c) bs |> Seq.toList with | [] -> Set.add c bs | (b, d)::_ -> h (Set.remove b bs) d
let w, cs, p = 4_000_000L, [|',';':';'=';' '|], Seq.fold h Set.empty
let us = System.IO.File.ReadAllLines("15") |> Seq.map (fun s -> let xs = s.Split(cs) in [for i in [3;6;13;16] -> int64 xs[i]])
let g m = us |> Seq.map (fun [x;y;i;j] -> let n = abs (x-i) + abs (y-j) - abs (y-m) in (x-n, x+n)) |> Seq.filter (fun (x, y) -> y >= x)
          |> p |> Seq.tryPick (fun (x, y) -> if x > 0 && x < w+2L then Some (x-1L) else if y > -2 && y < w then Some (y+1L) else None)
[0L..w] |> Seq.pick (fun m -> g m |> Option.map (fun x -> x * 4_000_000L + m))
