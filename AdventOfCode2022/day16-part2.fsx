// Runs below 3 seconds on my machine. A single CPU core is used.

// Variables:
// u, v, w: vertices (aka valves, ints)
// a, b: vertex sets (ints, bit masks)
// m, n: minutes (int)
// p, q, r: flow rates or total pressure amounts (ints)

let containsVertex a u = u < 32 && ((a >>> u) &&& 1) = 1
let removeVertex a u = a &&& ~~~(1 <<< u)

// Removes all vertices not in `a` from the graph, but makes sure the distances between all remaining vertices are the same as before.
let simplifyGraph a graph =
    let getNeighbors u =
        let mutable neighbors = Map.empty
        let rec explore m (v, n) =
            let m' = m + n
            match Map.tryFind v neighbors with
            | Some n' when m' >= n' -> ()
            | _ ->
                neighbors <- neighbors |> Map.remove v |> Map.add v m'
                if containsVertex a v && v <> u then () else Map.find v graph |> fst |> Seq.iter (explore m')
        explore 0 (u, 0)
        Map.toSeq neighbors |> Seq.filter (fst >> ((<>) u)) |> Seq.filter (fst >> containsVertex a)
    graph |> Map.toSeq |> Seq.filter (fst >> containsVertex a)
    |> Seq.map (fun (u, (_, p)) -> (u, (getNeighbors u |> Seq.toList, p))) |> Map.ofSeq

// maxA: the set of all vertices that have a non-zero flow rate.
let initialGraph, maxA =
    let lines = System.IO.File.ReadAllLines("16")
    let uMap = lines |> Seq.sortBy (fun s -> (s[23] = '0', s[6..7] <> "AA")) |> Seq.mapi (fun i s -> (s[6..7], i)) |> Map.ofSeq
    let uStart = uMap["AA"]
    let maxA = (1 <<< uStart) - 1
    lines |> Seq.map (fun s ->
        let u = uMap[s[6..7]]
        let xs = s[49..].Split(',') |> Seq.map (fun t -> (uMap[t.Trim()], 1)) |> Seq.toList
        let p = if u = uStart then 1 else s[23..24].TrimEnd(';') |> int
        (u, (xs, p))) |> Map.ofSeq |> simplifyGraph (maxA * 2 + 1), maxA

// In what follows we use the term "score" to represent the total amount of pressure released.
// For example, a vertex (valve) with a flow rate of 10 will give you a score of 50 if after this vertex is opened there are exactly
// five minutes left on the clock.

// Maps (u, a) pairs to lists of (m, p) pairs.
// Each (m, p) pair represents the following information: if the key of the pair is (u, a), then vertex `u` has already been visited
// once when there were `m` minutes left on the clock, the current score was `p`, and the set of vertices that still needed to be
// opened was equal to `a`.
// This information can be used to abort the current path early.
// For example, if the current state is (u, a, m, p) and this dictionary contains a pair (m, q) for key (u, a) with `q >= p` then
// it makes no sense to continue the current path.
// Likewise, if the current state is (u, a, m, p) and this dictionary contains a pair (n, q) for key (u, a) with `n <= m` and `q >= p` then
// again it makes no sense to continue the current path.
// If (m, p) and (n, q) are two pairs in one of the lists inside this dictionary then the following two statements hold:
// 1) m <> n, and
// 2) if m < n then p > q else q > p.
// Also, the pairs (m, p) in the lists are sorted by `m`.
let mem = System.Collections.Generic.Dictionary<int*int, (int*int) list>()

// The highest score that has yet been found.
let mutable best = 0

// u: the current location
// m: the number of minutes left
// a: the set of vertices that still need to be opened
// p: the current score
// i: a lower bound on the number of minutes that are needed to open the remaining vertices in set `a`
// j: after opening a vertex, `i` can be decremented by this amount
let rec walk graph u m a p i j =
    let list = match mem.TryGetValue((u, a)) with | (true, xs) -> xs | _ -> []
    match list |> Seq.tryFind (fst >> ((<=) m)) |> Option.map snd with
    | _ when m <= max i 1 -> ()
    | Some q when p <= q -> ()
    | _ ->
        mem[(u, a)] <- list |> List.filter (fun (n, q) -> n > m || q > p) |> (fun xs -> (m, p) :: xs) |> List.sortBy fst
        let q = Map.find u graph |> snd
        if containsVertex a u && q > 0 then
            let b = removeVertex a u
            let r = p + (m - 1) * q
            best <- max best r
            walk graph u (m - 1) b r (i - j) j
        Map.find u graph |> fst |> Seq.iter (fun (v, n) -> walk graph v (m - n) a p i j)

// Populates `best` with the best score that is possible if you open all the vertices in `a`, starting from vertex AA.
let computeBest a =
    mem.Clear()
    best <- 0
    let graph = simplifyGraph (a ||| (maxA + 1)) initialGraph
    let j = Map.values graph |> Seq.collect (fst >> Seq.map snd) |> Seq.sort |> Seq.tryHead |> Option.defaultValue 1 |> ((+) 1)
    let i = (graph.Count - 1) * j - 1
    walk graph (initialGraph.Count - 1) 26 a 0 i j

// bests: maps all sets of vertices `a` to the best possible scores if you open ALL vertices in `a`.
let bests = [0..maxA] |> Seq.map (fun a -> computeBest a; (a, best)) |> Map.ofSeq

// memo: will eventually map all sets of vertices `a` to the best possible scores if you only open vertices in `a` (you don't have
// to open all vertices in `a`).
let mutable memo = Map.empty
let getAllBelow a = [for u in [0..31] -> if containsVertex a u then Some (removeVertex a u) else None] |> List.choose id

// Returns the best possible score if you only open vertices in `a` (you don't have to open all vertices in `a`).
let rec solve a =
    match Map.tryFind a memo with
    | Some p -> p
    | None ->
        let q = Seq.max (bests[a] :: (getAllBelow a |> List.map solve))
        memo <- Map.add a q memo
        q

[0..maxA] |> Seq.map (fun a -> solve a + solve (maxA - a)) |> Seq.max
