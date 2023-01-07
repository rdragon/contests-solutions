let f g e = System.IO.File.ReadAllLines("23") |> Seq.iteri (fun y s -> Seq.iteri (fun x c -> if c='.' then () else g ((x+e/2)+(y+e/2)*e)) s)
let mutable o,us,xs,e = let e=10000 in Array.replicate (e*e) e,(let mutable us=[] in f (fun u -> us<-u::us) e;Array.ofSeq us),[0;2;3;1],e
let h, is, r = Seq.iteri (fun i w -> o[w] <- i) us; Array.map, [|0..us.Length-1|], fun m x -> ((m >>> (x*2)) &&& 7) = 0
let p u = Seq.fold (fun m v -> (m <<< 1) ||| (if o[v]=e then 0 else 1)) 0 [u-e-1;u-1;u+e-1;u+e;u+e+1;u+1;u-e+1;u-e;u-e-1]
let q u = let m = p u in if m = 0 then 0 else List.tryFind (r m) xs |> Option.map (fun x -> [|-e;1;e;-1|][x]) |> Option.defaultValue 0
let a (ys: int[]) = let g i = let y=ys[i] in if y=0 then 0 else let j=o[us[i]+y+y] in if j=e || ys[j] <> -y then y else 0 in h g is
let b (ys: int[]) = us <- let g i = let v = us[i] + ys[i] in if ys[i] = 0 then v else o[us[i]] <- e; o[v] <- i; v in h g is
let rec step n = let ys = a (h q us) in b ys; xs <- List.tail xs @ [List.head xs]; if Array.max ys = 0 then n else step (n+1) in step 1
