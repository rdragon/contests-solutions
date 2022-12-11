let f = function | 'X' -> 0 | 'Y' -> 3 | _ -> 6
let g = function | ("B X" | "A Y" | "C Z") -> 1 | ("C X" | "B Y" | "A Z") -> 2 | _ -> 3
System.IO.File.ReadAllLines("2") |> (fun x -> Seq.sumBy g x + Seq.sumBy (Seq.last >> f) x)
