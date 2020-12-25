let [|card; door|] = System.IO.File.ReadAllLines("Day25.txt") |> Array.map int64

let next prev subj = (prev * subj) % 20201227L
let rec loop acc enc =
        if acc = door then enc else loop (next acc 7L) (next enc card)

let part1 = loop (next 1L 7L) card

[<EntryPoint>]
let main _ = printfn "Part 1: %d" part1; printfn "Part 2: Merry Christmas!"; 0
