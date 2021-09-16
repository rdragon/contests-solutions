// 2021-08-16
let getDays year = function
    | 0 | 2 | 4 | 6 | 7 | 9 | 11 -> 31
    | 1 -> if year % 4 = 0 && year <> 1900 then 29 else 28
    | _ -> 30
let rec f year month dayOfWeek counter =
    if year = 2001 then
        counter
    else
        let counter' = counter + (if year > 1900 && dayOfWeek = 6 then 1 else 0)
        let days = getDays year month
        let dayOfWeek' = (dayOfWeek + days) % 7
        let year' = year + (if month = 11 then 1 else 0)
        let month' = (month + 1) % 12
        f year' month' dayOfWeek' counter'
f 1900 0 0 0