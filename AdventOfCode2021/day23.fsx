// The ps array that is the first argument of most functions contains the state of the map.
// The first 11 values in this array correspond to the hallway (from left to right).
// The last 16 values correspond to the side rooms (the left-most side room has indices 11, 15, 19, 23).
// A value of zero in this array corresponds to an empty space, the values 1 to 4 correspond to the amphipods A to D.

let room d p = d * 4 + 10 + p
let hallway p = p * 2
let steps i p d = abs (i - hallway p) + 1 + d
let isDone (ps: int[]) = [ 11 .. 26 ] |> List.forall (fun i -> ps[i] = ((i + 1) % 4) + 1)
let es = new System.Collections.Generic.Dictionary<int64 * int64, int option>()

let canMoveThroughHallway (ps: int[]) i j =
    let (i, j) = (min i j, max i j)
    [ for k in 1 .. 2 .. 9 -> k <= i || k >= j || ps[k] = 0 ] |> List.forall id

let canMoveToRoom (ps: int[]) i =
    let p = ps[i]
    let f d = ps[room d p] = 0 || ps[room d p] = p
    ps[room 0 p] = 0 && f 1 && f 2 && f 3 && canMoveThroughHallway ps i (hallway p)

let move (ps: int[]) i j e n =
    let p = ps[i]
    let ps = Array.copy ps
    Array.set ps i 0
    Array.set ps j p
    (ps, e + n * pown 10 (p - 1))

let getDepth (ps: int[]) p =
    [ 0 .. 3 ]
    |> List.takeWhile (fun d -> ps[room d p] = 0)
    |> List.length

let moveToRoom (ps: int[]) i e =
    let p = ps[i]
    let d = getDepth ps p - 1
    let j = room d p
    move ps i j e (steps i p d)

let canMoveToHallway (ps: int[]) p j =
    ps[j] = 0 && canMoveThroughHallway ps (hallway p) j

let shouldMoveOut (ps: int[]) p d i =
    ps[i] <> p || [ d + 1 .. 3 ] |> List.exists (fun d -> ps[room d p] <> p)

let rec g (ps: int[]) e =
    [
        for i in 0 .. 10 do
            if ps[i] > 0 && canMoveToRoom ps i then
                let (ps, e) = moveToRoom ps i e
                if isDone ps then Some(e) else f ps e
        for p in 1 .. 4 do
            let d = getDepth ps p
            let i = room d p
            if d < 4 && ps[i] > 0 && shouldMoveOut ps p d i then
                for j in [ 0; 1; 3; 5; 7; 9; 10 ] do
                    if canMoveToHallway ps p j then
                        move ps i j e (steps j p d) ||> f
    ]
    |> List.choose id
    |> (fun xs -> if List.isEmpty xs then None else Some(List.min xs))

and f (ps: int[]) e : int option =
    let rec h (ps: int[]) i j k x = if i > j then x else h ps (i + 1) j (k + 3) (x ||| (int64 ps[i] <<< k))
    let t = (h ps 0 19 0 0L, h ps 20 26 0 0L)
    if es.ContainsKey(t) then
        es[t] |> Option.map ((+) e)
    else
        let e' = g ps e
        es[t] <- (e' |> Option.map (fun x -> x - e))
        e'

let solve i ps =
    es.Clear()
    System.IO.File.ReadAllText("input.txt")
    |> Seq.toArray
    |> Array.map (fun c -> int c - 64)
    |> Array.filter (fun p -> p > 0 && p < 5)
    |> Array.insertManyAt i ps
    |> Array.append (Array.replicate 11 0)
    |> fun ps -> f ps 0

(solve 8 [ 1; 2; 3; 4; 1; 2; 3; 4 ], solve 4 [ 4; 3; 2; 1; 4; 2; 1; 3 ])
