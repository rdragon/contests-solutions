let mutable us, ss, h = [], System.IO.File.ReadAllLines("24"), fun m n -> let rec r x y = if y=0 then x else r y (x % y) in m * n / (r m n)
let m, n = ss |> Seq.iteri (fun y s -> Seq.iteri (fun x c -> if int c > 46 then us <- (x-1, y-1, c)::us else ()) s); ss[0].Length, ss.Length
let f (x, y, c) = ((x+(if c='>' then 1 else if c='<' then m-3 else 0))%(m-2), (y+(if c='v' then 1 else if c='^' then n-3 else 0))%(n-2), c)
let o' = List.init (m*n) (fun i -> let x, y = i%m, i/m in i=1 || not (x=0 || y=0 || x=m-1 || y=n-1))
let g _ = let mutable o = o' |> Array.ofList in Seq.iter (fun (x, y, _) -> o[x+1+(y+1)*m] <- false) us; o
let mutable is, o, os = Set.singleton 1, Array.empty, Array.init (h (m-2) (n-2)) (fun _ -> let o = g () in us <- List.map f us; o)
let p _ = is <- Seq.collect (fun i -> [i;i-1;i+1;i-m;i+m] |> List.filter (fun j -> j > 0 && o[j])) is |> Set.ofSeq
let rec q k = if is.Contains(m*n-2-m) then k else o <- os[k % os.Length]; p(); q (k+1) in q 1
