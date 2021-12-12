let f =
    Seq.toList
    >> (fun xs -> '9' :: xs @ ['9'])
    >> Seq.windowed 3
    >> Seq.map (fun [|x; y; z|] -> y < x && y < z)

let g xss =
    let yss = xss |> Seq.map f |> Seq.concat
    let zss = xss |> Seq.transpose |> Seq.map f |> Seq.transpose |> Seq.concat
    xss
    |> Seq.concat
    |> Seq.zip3 yss zss
    |> Seq.sumBy (fun (y, z, x) -> if y && z then int (string x) + 1 else 0)

System.IO.File.ReadAllLines("input.txt")
|> Array.map seq
|> g
