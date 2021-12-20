let enhance us n (vs: bool[,]) =
    let f i j _ =
        let rec g x =
            function
            | [] -> x
            | (b :: bs) -> g (2 * x + if b then 1 else 0) bs
        [ for i in i - 1 .. i + 1 do
            for j in j - 1 .. j + 1 ->
                min i j < 0 || max i j >= n || vs[i, j] ]
        |> g 0
        |> Array.get us
    Array2D.mapi f vs

let solve m =
    let ss = System.IO.File.ReadAllLines("input.txt")
    let n = ss.Length - 2 + m * 4
    let us = ss[0] |> Seq.map ((=) '#') |> Seq.toArray
    let f i j = min i j >= m * 2 && max i j < n - m * 2 && ss[i - m * 2 + 2][j - m * 2] = '#'
    let rec g k f = if k = 0 then id else f >> g (k - 1) f
    let vs = Array2D.init n n f |> g m (enhance us n)
    [ for i in m .. n - 1 - m do
        for j in m .. n - 1 - m do
            if vs[i, j] then 1 ]
    |> List.sum

(solve 2, solve 50)
