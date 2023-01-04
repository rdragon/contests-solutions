let ss, s', q = let xs = System.IO.File.ReadAllLines("22") in let n = Seq.length xs in xs[..n-3], xs[n-1], List.map
let z, t = let z = 2 + (ss |> Seq.map Seq.length |> Seq.max) in z, Array.replicate (z * (Seq.length ss + 2)) ' '
let m = ss |> Seq.iteri (fun y line -> Seq.iteri (fun x c -> t[(y+1)*z+x+1] <- c) line); [|1;z;-1;-z|]
let mutable i, e = Array.findIndex ((=) '.') t, 0
let p j a b k c d = let u,v = m[a],m[c] in List.zip (q (fun k -> (k,(b+2)%4)) [j..u..j+u*49]) (q (fun k -> (k,d)) [k..v..k+v*49])
let o = [[51;0;1;22952;1;0];[101;0;1;30553;0;3];[303;1;2;22901;3;2];[7853;0;3;7853;1;2];[23003;0;3;23003;1;2];[202;1;0;22800;3;0];
        [15250;3;0;15250;2;1]] |> Seq.collect (fun [j;a;b;k;c;d] -> p j a b k c d @ p k c d j a b) |> Map.ofSeq
let h _ = let u = m[e%4] in match Map.tryFind (i+u, e%4) o with | None -> (i+u, e) | Some (j, a) -> (j+m[a], a)
let g _ = let j, a = h () in if t[j] = '#' then () else i <- j; e <- a
let f = function | "R" -> e <- e+1 | "L" -> e <- e+3 | s -> Seq.iter g [1..int s]
System.Text.RegularExpressions.Regex.Split(s', "(R|L)") |> Seq.iter f; 1000*(i/z)+4*(i%z)+e%4
