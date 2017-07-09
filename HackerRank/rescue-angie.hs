-- 2016-04-26
-- vertices are numbered as follows, from left to right, top to bottom: 7, 6, 5, 8, 9, 4, 1, 2, 3

import Data.List

-- make one loop, ending at the original position. there is only one path, specified by "directions". for example, the route 2, 3, 4, 5 is denoted by RTU
loop [] = []
loop (d:ds) = d : loops ds ++ [rev d]

-- walk from first given vertex to last given vertex, and do not revisit the first vertex
walk [d1, d2] = [d1, d2]
walk (d:ds) = d : loops (init ds) ++ walk ds

many [] = []
many s = parens s ++ "*"

choose [s] = s
choose ss = parens $ intercalate "|" ss

parens s = '(' : s ++ ")"

-- any route that starts at 1 and ends by grabbing the princess
grabFrom1 = loopsFrom 1 ++ (concat $ zipWith (:) "RRTUL" (map loopsFrom [2..6])) ++ "P"

-- any loop that starts and ends at 6 and does not visit 2
loopFrom6Avoid2 = choose ["LR", loop "RDT"]

-- any route that starts at 6 and ends at 2 and visits 2 only once (at the end of the route)
goBackTo2 = loopsFrom6Avoid2 ++ choose ["LJD", walk "RDTL"]

-- loopFrom n: any loop that does not visit 1,2,..,n-1 and which starts and ends at n
loopFrom 1 = "R" ++ loopsFrom 2 ++ "L"
loopFrom 2 = choose ["UD", loopFromNoJump 2, walk "RTULLJ" ++ "D"]
loopFrom n = loopFromNoJump n

-- same as loopFrom, but we may not jump down from the cliff
loopFromNoJump n = loop $ drop (n - 2) "RTULL"

loops = many . loop
loopsFrom = many . loopFrom
loopsFrom6Avoid2 = many loopFrom6Avoid2

rev 'R' = 'L'
rev 'L' = 'R'
rev 'U' = 'D'
rev 'D' = 'U'
rev 'T' = 'T'

main = print $ '^' : grabFrom1 ++ goBackTo2 ++ loopsFrom 2 ++ "L$"