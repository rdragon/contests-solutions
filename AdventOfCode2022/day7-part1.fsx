let mutable a, ss = 0, Seq.append (System.IO.File.ReadAllLines("7")) (Seq.replicate 50 "$ cd ..") |> Seq.toList
let h = fun () -> let (t::ts) = ss in ss <- ts; t
let (|F|_|) (s : string) = if System.Char.IsDigit(s[0]) then Some(s.Split(' ')[0] |> int) else None
let rec g f d = match h() with | "$ cd .." -> d | F n -> g f (d + n) | s when s[2] = 'c' -> g f (d + f()) | _ -> g f d
let rec f = fun () -> let d = g f 0 in a <- a + (if d < 100_000 then d else 0); d
f(); a
