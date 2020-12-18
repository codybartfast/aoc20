open System
open System.Text.RegularExpressions
let inline digitToInt (c: char) = int c - int '0' |> int64

type Prev = Num of int64 | Op of (int64 -> int64 -> int64)

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day18.txt")

let rec readVal digits expr  =
    match expr with
    | d::rest when Char.IsNumber d -> 
        readVal  (d::digits) rest
    | _ ->
        if digits.IsEmpty then failwith "Oops" else
        digits |> List.rev |> List.toArray |> String |> int64, expr

let rec readOp acc expr = 
    match expr with
    | [] -> acc
    | ' '::c::' '::rest ->
    let op = match c with '+' -> (+) | '*' -> (*) | _ -> failwith "Oh Dear!"
    let v, rest = readVal [] rest
    readOp (op acc v) rest

let evalStr ln =
    let v, rest = ln |> toChars |> Array.toList |> (readVal [])
    readOp v rest

let rec depParens (str: string) =
    let simp = Regex.Replace(str, @"\(([\d +*]+)\)", (fun (m: Match) -> 
        m.Groups.[1].Value |> evalStr |> string))
    if simp = str then str else depParens simp


let part1 () = 
    lines |> Array.sumBy (depParens >> evalStr) 

    

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