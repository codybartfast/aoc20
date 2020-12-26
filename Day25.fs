let [|card; door|] = System.IO.File.ReadAllLines("Day25.txt") |> Array.map int64

let itr n subj = (n * subj) % 20201227L
let rec loop trial key =
        if trial = door then key else loop (itr trial 7L) (itr key card)

let part1 = loop (itr 1L 7L) card

[<EntryPoint>]
let main _ = printfn "Part 1: %d" part1; printfn "Part 2: Merry Christmas!"; 0

(* Golfier

let [|c; d|] = System.IO.File.ReadAllLines("Day25.txt") |> Array.map int64
let i n s = n * s % 20201227L
let rec l t k = if t = d then k else l (i t 7L) (i k c)
l (i 1L 7L) c

*)
