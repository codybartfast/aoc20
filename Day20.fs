open System
open System.Text.RegularExpressions
open Grid


let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}") |> Array.toList
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let toNum (line: char[]) =
    line
    |> Array.map (function '.' -> '0' | _ -> '1') 
    |> String 
    |> (fun s -> Convert.ToInt32(s, 2))

let toPair (line: char[]) = toNum line, (line |> Array.rev |> toNum)

let blocks = System.IO.File.ReadAllText("Day20.txt") |> toBlocks
let parse block =
    let head::rest = toLines block
    let id = Regex.Match(head, "\d+").Value |> int64
    let grid = rest |> List.toArray |> Grid.ofLines
    id, 
        [grid.Row 0; grid.Col 9; grid.Row 9; grid.Col 0]
        |> List.map toPair
let tiles = blocks |> List.map parse

let top (_, [t; r; b; l]) = t |> fst
let right (_, [t; r; b; l]) = r  |> fst
let bottom (_, [t; r; b; l]) = b  |> fst
let left (_, [t; r; b; l]) = l  |> fst
let faces = [(top, bottom); (right, left); (bottom, top); (left, right)]

let rotate (id, [(a, ra); (b, rb); (c, rc); (d, rd)]) =
    (id, [(rd, d); (a, ra); (rb, b); (c, rc)])
let flip (id, [(a, ra); (b, rb); (c, rc); (d, rd)]) =
    (id, [(ra, a); (d, rd); (rc, c); (b, rb)])

let variations tile =
    [rotate; rotate; rotate; (rotate >> flip); rotate; rotate; rotate]
    |> List.scan (fun tile transform -> transform tile) tile

let canMatch tile1 tile2 =
    let t2Vars = tile2 |> variations
    tile1
    |> variations
    |> List.collect(fun t1 -> t2Vars |> List.map (fun t2 -> (t1, t2)))
    |> List.exists(fun (t1, t2) -> faces |> List.exists (fun (f1, f2) -> f1 t1 = f2 t2))

let matchCount tiles tile =
    tiles
    |> List.filter ((<>) tile)
    |> List.filter (canMatch tile)
    |> List.length

let part1 () = 
    tiles 
    |> List.filter ((matchCount tiles) >> ((= ) 2))
    |> List.map fst
    |> List.reduce (*)
    

let part2 () = "?"
    // getMatches tiles tiles.[1]

[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0