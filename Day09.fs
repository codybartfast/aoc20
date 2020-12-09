open System
open System.Text.RegularExpressions

let rec pairCombos = function
    | [] | [_] -> []
    | head::tail ->
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day09.txt")
let parse (ln: string) = int64 ln
let input = lines |> Array.map parse |> Array.toList
let poolSize = 25
let pool, stream = input |> List.take poolSize, input |> List.skip poolSize

let isValid (pool, stream) =
    pairCombos pool
    |> List.map (fun (a, b) -> a + b)
    |> (List.contains (List.head stream))

let replace _new lst =  (List.tail lst) @ [_new]

let rec skipValid (pool, stream) =
    if not (isValid (pool, stream)) then stream else
    skipValid (replace (List.head stream) pool, List.tail stream) 

let part1 =
    skipValid (pool, stream)
    |> List.head

let contiguous target stream =
    let nums = Array.ofList stream
    let len = nums.Length
    seq{ for i in 0 .. len - 2 do 
            for j in i + 1 .. len - 1 do
                let sum = [i .. j] |> List.sumBy (fun i -> nums.[i])
                if sum <> target then yield None else
                let cont = Array.sub nums i (j - i)
                yield Some (Array.max cont + Array.min cont)}

let part2 () = 
    Seq.pick id (contiguous part1 (pool@stream))

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1); printfn "Part 2: %A" (part2 ()); 0
