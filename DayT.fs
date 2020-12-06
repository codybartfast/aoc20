open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day0.txt") |> Array.toList
let parse (ln: string) = ln //.Split(' ')
let input = lines |> List.map parse



let part1 =
    input

let part2 =
    "?"

[<EntryPoint>]
let main _ = part1 |> printfn "Part 1: %A"; part2 |> printfn "Part 2: %A"; 0
