let start =
    System.IO.File.ReadAllLines("Day15.txt").[0].Split(',') |> Array.map int
let startMap =
    start.[0..(start.Length - 2)] |> Array.mapi (fun i n -> (n, i + 1))  |> Map

let play rounds =
    let rec play  max last len (seen: Map<int, int>) =
        if max = (len ) then last else
        let next = match seen.TryFind last with| None -> 0 | Some n -> len - n
        play max next (len + 1) (seen.Add (last, len ))
    play rounds (Array.last start) (start.Length) startMap

let part1 () = play 2020
let part2 () = play 30_000_000

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
