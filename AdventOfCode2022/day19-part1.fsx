let mutable a, b, s, ss, o, c, t, k = 0, [||], [||], [], [||], [|2;6;2|], [|1;0;0;0;0;0;0;0;24|], [|3;5;3;1|]
let d, r, h = Array.map int, (fun _ -> s <- List.head ss; ss <- List.tail ss), fun i -> i = 3 || s[i] < o[i] && s[4+i] < o[i] * (s[8]-c[i])
let w i = let p j = if b[i*3+j]=0 then 0 else (if s[j]=0 then 99 else max 0 ((b[i*3+j]-s[4+j]+s[j]-1)/s[j])) in Seq.max [p 0; p 1; p 2]
let v n i = let x, p = s[7] + (if i = 3 then s[8]-n else 0), fun j -> s[j] + (if i = j then 1 else 0)
            let q j = s[4+j] + s[j]*n - b[i*3+j] in ss <- s :: ss; s <- [|p 0;p 1;p 2;p 3;q 0;q 1;q 2;x;s[8]-n|]; a <- max a s[7]
let u g i = if h i then (let n = w i + 1 in if s[8] - n >= k[i] then v n i; g (); r () else ()) else ()
let f x = let rec g _ = [0..3] |> Seq.iter (u g) in b<-d x; s<-t; o<-[|for i in [0..2] -> Seq.max [b[i];b[i+3];b[i+6];b[i+9]]|]; a<-0; g(); a
System.IO.File.ReadAllLines("19") |> Seq.map (fun x -> let y,z = x.Split(' '),"0" in [|y[6];z;z;y[12];z;z;y[18];y[21];z;y[27];z;y[30]|] |> f)
|> Seq.indexed |> Seq.sumBy (fun (i, x) -> x * (i + 1))

// a: best score
// b: blueprint, array of length 12: three numbers (ore, clay, obsidian) for each robot
// s: state, array of length 9: 4 robots; 4 resources; minutes left. for geodes we do as if the robot creates all its geodes right away
// ss: history of states
// o: for each of the first three resources, the maximum possible consumption per minute of this resource
// c: for each of the first three resources, the number of minutes at the end during which the generation of this resource is wasteful
// t: the starting state
// k: for each robot, the minimum time that is needed after the creation of this robot before it can help produce a geode
// r: restore the last state (undo the last `v` call)
// h: returns whether it currently makes sense to create another robot of type `i`
// w: returns the number of minutes you currently need to wait before enough resources are available to construct a robot of type `i`
// v: simulates `n` minutes in which only in the last minute a new robot is created (of type `i`). updates `s` and `a`
