// 2021-08-15
let startDate = System.DateTime(1901, 1, 1)
let endDate = System.DateTime(2000, 12, 31)
let rec f date counter =
    if date = endDate then
        counter
    else
        let counter' = counter + (if date.Day = 1 && date.DayOfWeek = System.DayOfWeek.Sunday then 1 else 0)
        f (date.AddDays 1.0) counter'
f startDate 0