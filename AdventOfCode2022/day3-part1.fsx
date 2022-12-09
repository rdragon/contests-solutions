let f = Seq.map Set.ofSeq >> Set.intersectMany >> Seq.exactlyOne >> int >> (fun x -> if x < 91 then x - 38 else x - 96)
System.IO.File.ReadAllLines "3" |> Seq.sumBy (Seq.splitInto 2 >> f)
