let ss, s' = let xs = System.IO.File.ReadAllLines("22") in let n = Seq.length xs in xs[..n-3], xs[n-1]
let z, t = let z = 2 + (ss |> Seq.map Seq.length |> Seq.max) in z, Array.replicate (z * (Seq.length ss + 2)) ' '
let m = ss |> Seq.iteri (fun y line -> Seq.iteri (fun x c -> t[(y+1)*z+x+1] <- c) line); [|1;z;-1;-z|]
let mutable i, e = Array.findIndex ((=) '.') t, 0
let h _ = let u = m[e%4] in u + (if t[i + u] <> ' ' then i else Seq.find (fun l -> t[l] = ' ') (seq {i..(-u)..i-u*999}))
let g _ = let j = h () in if t[j] = '#' then () else i <- j
let f = function | "R" -> e <- e+1 | "L" -> e <- e+3 | s -> Seq.iter g [1..int s]
System.Text.RegularExpressions.Regex.Split(s', "(R|L)") |> Seq.iter f; 1000*(i/z)+4*(i%z)+e%4
