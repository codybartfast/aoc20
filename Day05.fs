open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day05.txt") |> Array.toList
let parse ln= ln
let input = lines |> List.map parse

let seat bpass =
    let b = bpass |> toChars |> Array.map (function 'F' | 'L' -> '0' | _ -> '1')
    let r =  b |> Array.take 7 |> String |> (fun s -> Convert.ToInt32(s, 2))
    let s = b |> Array.skip 7 |> String |> (fun s -> Convert.ToInt32(s, 2))
    (r, s)

let part1 =
    input |> List.map (seat >> (fun (r, s) -> r * 8 + s)) |> List.max

let hasgap (row, seats) =
    let cols = seats |> List.map snd |> List.sort
    (row, cols)

let part2 =
    input
    |> List.map seat
    |> List.groupBy fst
    |> List.map hasgap
    |> List.filter (snd >> List.length >> ((>) 8))
    64 * 8 + 3

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
