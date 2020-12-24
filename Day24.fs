open System
open System.Text.RegularExpressions

let (++) (x, y) (x', y') = (x + x', y + y')

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let e, se, sw, w, nw, ne = (2, 0), (1 ,1),(-1, 1), (-2, 0), (-1, -1), (1, -1)
let dirs = [e; se; sw; w; nw; ne]

let lines = System.IO.File.ReadAllLines("Day24.txt")
let parse (ln: string) =
    let cs = Regex.Matches(ln, @"(e|se|sw|w|nw|ne)")
    cs |> Seq.map (fun c -> c.Value) |> Seq.toList
    |> List.map (function
        | "e" -> e | "se" -> se | "sw" -> sw
        | "w" -> w | "nw" -> nw | "ne" -> ne)
let tile path =
    ((0, 0), path) ||> List.fold (++)
let paths = lines |> Array.map parse |> Array.toList
let isBlack n = n % 2 = 1

let flip floor crd = 
    let tile = 
        match Map.tryFind crd floor with 
        | None | Some false -> true
        | Some true -> false
    floor.Add (crd, tile)

let flipPath floor path = flip floor (tile path)

let part1 () =
    (Map.empty, paths) ||> List.fold flipPath 
    |> Map.toSeq |> Seq.filter snd |> Seq.length
        
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