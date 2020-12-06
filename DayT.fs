open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day0.txt") |> Array.toList
let parse (ln: string) = ln //.Split(' ')
let input = lines |> List.map parse



let part1 =
    input

let part2 =
    "?"

[<EntryPoint>]
let main _ = printfn "Part 1: %A" part1; printfn "Part 2: %A" part2; 0
