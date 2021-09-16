// 2021-08-24

let read s =
    let rec f n =
        function
        | [] -> n
        | ('I' :: 'V' :: cs) -> f (n + 4) cs
        | ('I' :: 'X' :: cs) -> f (n + 9) cs
        | ('X' :: 'L' :: cs) -> f (n + 40) cs
        | ('X' :: 'C' :: cs) -> f (n + 90) cs
        | ('C' :: 'D' :: cs) -> f (n + 400) cs
        | ('C' :: 'M' :: cs) -> f (n + 900) cs
        | ('I' :: cs) -> f (n + 1) cs
        | ('V' :: cs) -> f (n + 5) cs
        | ('X' :: cs) -> f (n + 10) cs
        | ('L' :: cs) -> f (n + 50) cs
        | ('C' :: cs) -> f (n + 100) cs
        | ('D' :: cs) -> f (n + 500) cs
        | ('M' :: cs) -> f (n + 1000) cs
    f 0 (Seq.toList s)

let write n =
    let rec f cs n =
        if n >= 1000 then f ('M' :: cs) (n - 1000)
        elif n >= 900 then f ('M' :: 'C' :: cs) (n - 900)
        elif n >= 500 then f ('D' :: cs) (n - 500)
        elif n >= 400 then f ('D' :: 'C' :: cs) (n - 400)
        elif n >= 100 then f ('C' :: cs) (n - 100)
        elif n >= 90 then f ('C' :: 'X' :: cs) (n - 90)
        elif n >= 50 then f ('L' :: cs) (n - 50)
        elif n >= 40 then f ('L' :: 'X' :: cs) (n - 40)
        elif n >= 10 then f ('X' :: cs) (n - 10)
        elif n >= 9 then f ('X' :: 'I' :: cs) (n - 9)
        elif n >= 5 then f ('V' :: cs) (n - 5)
        elif n >= 4 then f ('V' :: 'I' :: cs) (n - 4)
        elif n >= 1 then f ('I' :: cs) (n - 1)
        else Seq.rev cs |> Seq.toArray |> System.String
    f [] n

let text = System.Net.Http.HttpClient().GetStringAsync("https://projecteuler.net/project/resources/p089_roman.txt").Result

let diff s = Seq.length s - (s |> read |> write).Length

System.Text.RegularExpressions.Regex.Matches(text, @"\b[A-Z]+\b")
|> Seq.sumBy (fun m -> m.Value |> diff)