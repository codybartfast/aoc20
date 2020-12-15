let start =
    System.IO.File.ReadAllLines("Day15.txt").[0].Split(',') |> Array.map int

let game rounds =
    let seen = Array.zeroCreate rounds
    start.[0..(start.Length - 2)] |> Array.iteri (fun i n -> seen.[n] <- i + 1)

    let rec play prev len =
        if len = rounds then prev else
        let next = match seen.[prev] with 0 -> 0 | n -> len - n
        seen.[prev] <- len
        play next (len + 1)
    play (Array.last start) start.Length

let part1 () = game 2020
let part2 () = game 30_000_000

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
