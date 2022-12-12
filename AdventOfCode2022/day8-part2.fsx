let h c xs = Seq.takeWhile ((>) c) xs |> Seq.length |> ((+) 1) |> min (Array.length xs)
let f = Array.map (fun xs -> Array.mapi (fun i (c, ts) -> (c, (xs |> Array.map fst |> Array.skip (i+1) |> h c) :: ts)) xs)
let g = Array.transpose >> f >> Array.map Array.rev >> f
let u = Seq.map (fun c -> (int c, [])) >> Array.ofSeq
System.IO.File.ReadAllLines("8") |> Array.map u |> g |> g |> Seq.collect id |> Seq.map (snd >> Seq.reduce (*)) |> Seq.max
