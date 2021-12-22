// # Idea
// For each cuboid in the initialization procedure that turns on cubes do the following.
// Find "the" parts (cuboids) of the current cuboid that are not contained inside any other cuboid in the initialization
// procedure that come after the current cuboid (including cuboids that turn off cubes).
// Measure the number of cubes inside each part and take the sum.
// 
// So each cuboid that turns on cubes now has a number associated with it.
// The answer of the puzzle is the sum of these numbers.
//
// The hardest part is finding "the" parts (cuboids) of the current cuboid that are not contained inside other cuboids.
// This is done one cuboid at a time.
// For each cuboid that should be removed from the current cuboid, the 3-dimensional grid is split into 6 disjoint regions:
// 1. The region of all cubes that are higher than the cuboid to remove (all cubes with a z value larger than the z2 of the cuboid).
// 2. The region of all cubes that are lower than the cuboid to remove (all cubes with a z value smaller than the z1 of the cuboid).
// 3. The region of all cubes that both have an x value that is smaller than the x1 of the cuboid and that are not contained inside regions 1 and 2.
// 4. The region of all cubes that both have an x value that is larger than the x2 of the cuboid and that are not contained inside regions 1, 2, and 3.
// 5. The region of all cubes that both have an y value that is smaller than the y1 of the cuboid and that are not contained inside regions 1, 2, 3, and 4.
// 6. The region of all cubes that both have an y value that is larger than the y2 of the cuboid and that are not contained inside regions 1, 2, 3, 4, and 5.
// Then the current cuboid is split into at most 6 parts (cuboids) by taking the intersection of the current cuboid with each of these regions individually.
// These parts are not contained inside the cuboid to remove, and therefore we can now continue to the next cuboid to remove, for which we
// do the same thing, except that we do not split the original current cuboid into parts, but we split each part into at most 6 new parts.

open System.Text.RegularExpressions

let hasOverlap (x1, x2, y1, y2, z1, z2) (x1', x2', y1', y2', z1', z2') =
    x2 >= x1' && x1 <= x2' && y2 >= y1' && y1 <= y2' && z2 >= z1' && z1 <= z2'

let getSize (x1, x2, y1, y2, z1, z2) = bigint(x2 - x1 + 1) * bigint(y2 - y1 + 1) * bigint(z2 - z1 + 1)

let removeCuboid q p =
    if not (hasOverlap p q) then
        [p]
    else
        let (x1, x2, y1, y2, z1, z2) = p
        let (x1', x2', y1', y2', z1', z2') = q
        [
            if z2 > z2' then (x1, x2, y1, y2, max z1 (z2' + 1), z2)
            if z1 < z1' then (x1, x2, y1, y2, z1, min z2 (z1' - 1))
            if z2 >= z1' && z1 <= z2' then
                let (z1'', z2'') = (max z1 z1', min z2 z2')
                if x1 < x1' then (x1, min x2 (x1' - 1), y1, y2, z1'', z2'')
                if x2 > x2' then (max x1 (x2' + 1), x2, y1, y2, z1'', z2'')
                if x2 >= x1' && x1 <= x2' then
                    let (x1'', x2'') = (max x1 x1', min x2 x2')
                    if y1 < y1' then (x1'', x2'', y1, min y2 (y1' - 1), z1'', z2'')
                    if y2 > y2' then (x1'', x2'', max y1 (y2' + 1), y2, z1'', z2'')
        ]

let rec removeCuboids ps = function
    | [] -> ps |> List.map getSize |> List.sum
    | (q :: qs) -> removeCuboids (ps |> List.map (removeCuboid q) |> List.concat) qs

let rec solve n =
    function
    | [] -> n
    | ((b, p) :: xs) -> solve (n + if b then removeCuboids [p] (xs |> List.map snd) else 0I) xs

let read s =
    Regex.Matches(s, "[0-9-]+")
    |> Seq.map (string >> int)
    |> Seq.toArray
    |> (fun xs -> (xs[0], xs[1], xs[2], xs[3], xs[4], xs[5]))

System.IO.File.ReadAllLines("input.txt")
|> Array.map (fun s -> (s[1] = 'n', read s))
|> Array.toList
|> solve 0I
