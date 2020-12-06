open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray()

let txt = System.IO.File.ReadAllText("Day06.txt")
let parse (txt: string) =  
    Regex.Split(txt.Trim(), @"(?:\r?\n){2,}")
    |> Array.map (fun lns -> Regex.Split(lns, @"\s+") |> Array.map toChars)

let groups = parse txt 



let part1 =
    groups |> Array.sumBy Array.length

let part2 =
    "?"

[<EntryPoint>]
let main _ = part1 |> printfn "Part 1: %A"; part2 |> printfn "Part 2: %A"; 0
