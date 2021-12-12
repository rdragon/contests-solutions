let g =
    function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | c -> c

let h c = 1 + Seq.findIndex ((=) c) ")]}>"

let rec f ys =
    function
    | [] -> ys |> Seq.map (h >> int64) |> Seq.reduce (fun x y -> x * 5L + y)
    | (x :: xs) ->
        if g x <> x then
            f (g x :: ys) xs
        elif x = List.head ys then
            f (List.tail ys) xs
        else
            0L

System.IO.File.ReadAllLines("input.txt")
|> Array.map (Seq.toList >> f [])
|> Array.filter ((<>) 0)
|> Array.sort
|> (fun xs -> xs[xs.Length / 2])
