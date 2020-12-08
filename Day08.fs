let parse (ln: string) =
    let flds = ln.Split(' ')
    flds.[0], int flds.[1]
let game = System.IO.File.ReadAllLines("Day08.txt") |> Array.map parse
let gameLen = game.Length

let run program =
    let rec run seen acc pos =
        if Set.contains pos seen || pos >= gameLen then acc, pos else
        let ins, n = Array.item pos program
        let seen = Set.add pos seen
        match ins with
        | "acc" -> run seen (acc + n) (pos + 1)
        | "jmp" -> run seen acc (pos + n)
        | "nop" -> run seen acc (pos + 1)
    run Set.empty 0 0

let modGame modPos =
    let modded = Array.copy game
    let (oldInst, n) = modded.[modPos]
    let newInst = match oldInst with "jmp" -> "nop" | "nop" -> "jmp" | i -> i
    modded.[modPos] <- (newInst, n)
    modded

let part1 = run (Array.copy game) |> fst
let part2 =
    [0 .. gameLen - 1]
    |> List.map (modGame >> run)
    |> List.filter (snd >> ((=) gameLen))
    |> List.exactlyOne
    |> fst

[<EntryPoint>]
let main _ = printfn "Part 1: %A" part1; printfn "Part 2: %A" part2; 0
