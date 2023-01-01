type U = {I: int64; mutable L: U; mutable R: U}
let us = let rec v = {I=0;L=v;R=v} in System.IO.File.ReadAllLines("20") |> Array.map (fun s -> {I=int64 s * 811589153L;L=v;R=v})
let n, h = us.Length - 1, fun u v -> u.R <- v; v.L <- u
Seq.iter (fun i -> h us[i] us[(i + 1) % (n + 1)]) [0..n]
let rec g u i = if i = 0 then u else g (u.R) (i - 1)
let f u = let i, v = (int (u.I % int64 n) + n) % n, u.L in h v u.R; let w = g v i in h u w.R; h w u 
Seq.iter (fun _ -> Seq.iter f us) [0..9]; List.sumBy (fun i -> Seq.find (fun u -> u.I = 0) us |> (fun u -> (g u i).I)) [1000;2000;3000]
