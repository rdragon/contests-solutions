// 2021-08-18
open System.Text.RegularExpressions
let text = (new System.Net.Http.HttpClient()).GetStringAsync("https://projecteuler.net/project/resources/p042_words.txt").Result
let getWordValue : string -> int = Seq.sumBy (fun c -> int c - int 'A' + 1)
let triangleNumbers = [2..100] |> List.scan (+) 1 |> Set.ofList
Regex.Matches(text, @"\b[A-Z]+\b") |> Seq.filter (fun x -> x.Value |> getWordValue |> triangleNumbers.Contains) |> Seq.length