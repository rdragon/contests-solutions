let mutable ds, ss = [], Seq.append (System.IO.File.ReadAllLines("7")) (Seq.replicate 50 "$ cd ..") |> Seq.toList
let h = fun () -> let (t::ts) = ss in ss <- ts; t
let (|F|_|) (s : string) = if System.Char.IsDigit(s[0]) then Some(s.Split(' ')[0] |> int) else None
let rec g f d = match h() with | "$ cd .." -> d | F n -> g f (d + n) | s when s[2] = 'c' -> g f (d + f()) | _ -> g f d
let rec f = fun () -> let d = g f 0 in ds <- d::ds; d
f() |> (fun x -> ds |> Seq.sort |> Seq.find (fun d -> x - d <= 40_000_000))
