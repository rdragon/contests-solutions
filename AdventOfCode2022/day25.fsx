let f = let p x c = x * 5L + match c with | '=' -> -2L | '-' -> -1 | _ -> string c |> int64 in Seq.fold p 0
let rec g cs x = if x = 0L then cs else g ("012=-"[int (x % 5L)] :: cs) (x / 5L + (if x % 5L > 2 then 1L else 0L))
System.IO.File.ReadAllLines("25") |> Seq.map f |> Seq.sum |> g [] |> Array.ofList |> System.String
