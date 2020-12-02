open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day02.txt") |> Array.toList
let parse (ln: string) = 
    let m = Regex.Match(ln, @"(\d+)-(\d+) (\w): (.*)")
    ((m.Groups.[3].Value.[0], (int m.Groups.[1].Value, int m.Groups.[2].Value)),
        m.Groups.[4].Value)
let input = lines |> List.map parse

let validDownTheStreet ((c, (mn, mx)), (pwd: string)) =
    pwd.ToCharArray()
    |> Array.groupBy id
    |> Map
    |> (fun map -> 
        match Map.tryFind c map with
        | None -> false
        | Some arr -> mn <= arr.Length && arr.Length <= mx)

let validHere ((c, (mn, mx)), (pwd: string)) =
    let a = pwd.[mn - 1] = c
    let b = pwd.[mx - 1] = c
    (a && (not b)) || ((not a) && b)

let part1 =
    input |> List.filter validDownTheStreet  |> List.length

let part2 =
    input |> List.filter validHere |> List.length

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
