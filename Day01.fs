open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray() 

let expns = System.IO.File.ReadAllLines("Day01.txt") |> Array.map int
let lst = expns.Length - 1

let part1 =
    [ for i in 0 .. lst do for j in i + 1 .. lst do
        let a, b = expns.[i], expns.[j]
        if a + b = 2020 then
            (a * b) ].Head

let part2 =
    [ for i in 0 .. lst do for j in i + 1 .. lst do for k in j + 1 .. lst do
        let a, b, c = expns.[i], expns.[j], expns.[k]
        if a + b + c = 2020 then
            (a * b * c) ].Head

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
