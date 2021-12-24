let ts =
    System.IO.File.ReadAllText("input.txt")
    |> fun s -> System.Text.RegularExpressions.Regex.Matches(s, "[0-9-]+")
    |> Seq.map (string >> int64)
    |> Seq.splitInto 14
    |> Seq.map (fun xs -> (xs[2], xs[3], xs[9]))
    |> Seq.toArray

let step z d (a, b, c) =
    if z % 26L = d - b then
        z / a
    else
        d + c + 26L * (z / a)

let rs = new System.Collections.Generic.Dictionary<int64 * int, int list option>()

let rec compute ds z n =
    seq {
        for d in ds ->
            let z = step z (int64 d) ts[14 - n]
            find ds z (n - 1)
            |> Option.map (List.append [d]) }
    |> Seq.choose id
    |> Seq.tryHead

and find ds z =
    function
    | 0 -> if z = 0L then Some [] else None
    | n ->
        let x = (z, n)
        if rs.ContainsKey(x) then
            rs[x]
        else
            let r = compute ds z n
            rs[x] <- r
            r

let solve f =
    rs.Clear()
    find (f [ 1 .. 9 ]) 0L 14

(solve List.rev, solve id)
