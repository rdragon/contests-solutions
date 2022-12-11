let f = function | 'X' -> 1 | 'Y' -> 2 | _ -> 3
let g = function | ("A X" | "B Y" | "C Z") -> 3 | ("A Y" | "B Z" | "C X") -> 6 | _ -> 0
System.IO.File.ReadAllLines("2") |> (fun x -> Seq.sumBy g x + Seq.sumBy (Seq.last >> f) x)
