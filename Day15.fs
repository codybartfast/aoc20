let start = System.IO.File.ReadAllText("Day15.txt").Split(',') |> Array.map int

let inline next (seen: int[]) spoken round =
    let nxt = match seen.[spoken] with 0 -> 0 | prev -> round - prev
    seen.[spoken] <- round; nxt

let play rounds =
    let seen = Array.zeroCreate rounds
    start.[0..(start.Length - 2)] |> Array.iteri (fun i n -> seen.[n] <- i + 1)
    (Array.last start, seq{start.Length..(rounds - 1)}) ||> Seq.fold (next seen)
let sw = System.Diagnostics.Stopwatch ()
sw.Start ()

let part1 () = play 2020
let part2 = play 30_000_000
sw.Stop ()
printfn "%A" sw.Elapsed.TotalSeconds

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ); 0
