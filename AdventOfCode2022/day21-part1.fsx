let d = System.IO.File.ReadAllLines("21") |> Seq.map (fun s -> (s[0..3], s[6..].Split(' ') |> List.ofSeq)) |> Map.ofSeq
let g = function | "+" -> (+) | "-" -> (-) | "*" -> (*) | _ -> (/)
let rec f i = match d[i] with | [s] -> int64 s | [j;c;k] -> g c (f j) (f k) in f "root"
