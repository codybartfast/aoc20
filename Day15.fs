open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let starting = 
    System.IO.File.ReadAllLines("Day15.txt").[0].Split(',')
    |> Array.map int
    |> Array.toList
    |> List.rev

let next (last::rest) =
    match List.tryFindIndex ((=) last) rest with
    | None -> (0::last::rest)
    | Some n -> ((n + 1)::last::rest)
    
let part1 () =
    (starting, [starting.Length + 1 .. 2020])
    ||> List.fold (fun game _ ->  next game)
    |> List.head
    

let rec play  max last len (seen: Map<int, int>) =
    // printfn "max:%d, len:%d, last:%d, map:%A" max len last seen
    if max = (len - 1) then last else
    let next = match seen.TryFind last with| None -> 0 | Some n -> len - 1 - n
    play max next (len + 1) (seen.Add (last, len - 1))

let startMap = starting |> List.tail |> List.rev |> List.mapi (fun i n -> (n, i + 1)) |> Map

let part2 () =
    play 30_000_000 starting.Head (starting.Length + 1) startMap 
    

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
