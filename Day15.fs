let start =
    System.IO.File.ReadAllLines("Day15.txt").[0].Split(',') |> Array.map int

let game rounds =
    let seen = Array.zeroCreate rounds
    start.[0..(start.Length - 2)] |> Array.iteri (fun i n -> seen.[n] <- i + 1)

    let rec play spoken round =
        if round = rounds then spoken else
        let next = match seen.[spoken] with 0 -> 0 | prev -> round - prev
        seen.[spoken] <- round
        play next (round + 1)
    play (Array.last start) start.Length

let part1 () = game 2020
let part2 () = game 30_000_000

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
