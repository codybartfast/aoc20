open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day25.txt")
let cardPub = int64 lines.[0]
let doorPub = int64 lines.[1]

let next p subj = (p * subj) % 20201227L

let search pub =
    printfn "Searching for %d" pub
    let rec loop l p =
        let r = next p 7L
        if r = pub then l else loop (l + 1L) r
    loop 1L 1L



let part1 () =
    let cardLoop, doorLoop = search cardPub, search doorPub
    (cardPub, seq{2L .. doorLoop}) ||> Seq.fold (fun p _ -> next p cardPub)

let part2 () =
    "?"

[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0