open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day08.txt")
let parse ln = 
    let flds = Regex.Split(ln, @" ")
    flds.[0], int flds.[1]
let input = lines |> Array.map parse
let len = input.Length

let rec apply (program: (string * int)[]) seen acc pos =
    if Set.contains pos seen || pos >= len then acc, pos else
    let ins, n = program.[pos]
    let seen = Set.add pos seen
    match ins with
    | "acc" -> apply program seen (acc + n) (pos + 1)
    | "jmp" -> apply program seen acc (pos + n)
    | "nop" -> apply program seen acc (pos + 1)

let part1 =
    apply (Array.copy input) Set.empty 0 0 |> fst

let part2 =
    [0 .. len - 1]
    |> List.map (fun mpos ->
        let modded = Array.copy input
        let old, n = modded.[mpos]
        let newinst = match old with "jmp" -> "nop" | "nop" -> "jmp" | i -> i
        modded.[mpos] <- newinst, n
        apply modded Set.empty 0 0)
    |> List.filter (snd >> ((=) len))

[<EntryPoint>]
let main _ = printfn "Part 1: %A" part1; printfn "Part 2: %A" part2; 0
