let xs = System.IO.File.ReadAllText("input.txt").Split(',') |> Array.map int

let f i =
    xs
    |> Array.sumBy (fun j ->
        let n = abs (i - j)
        n * (n + 1) / 2)

Array.init 1000 f |> Array.min
