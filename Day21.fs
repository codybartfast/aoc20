open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day21.txt")
let parse (ln: string) =
    let m = Regex.Match(ln, @"((?<i>\w+) )+\(contains ((?<a>\w+)[, ]*)+")
    let ingrds = m.Groups.["i"].Captures |> Seq.map (fun c -> c.Value) |> Set
    let allgns = m.Groups.["a"].Captures |> Seq.map (fun c -> c.Value) |> Set
    ingrds, allgns
let foods = lines |> Array.map parse |> Array.toList

let ingredients = foods |> List.map fst |> Set.unionMany
let allergens = foods |> List.map snd |> Set.unionMany
let informtion = allergens |> Seq.map (fun a -> (a, ingredients)) |> Map
let candidates = fst
let excluded = snd

let analyse info (ingrds, allgns) =
    (info, allgns)
    ||> Seq.fold( fun info allgn ->
        Map.add allgn (Set.intersect info.[allgn] ingrds) info)
    
let firstpass = (informtion, foods) ||> List.fold analyse
let suspect =  firstpass |> Map.toSeq |> Seq.map snd |> Set.unionMany
let free = foods |> List.map (fst >> (fun ingrds -> Set.difference ingrds suspect))

let part1 () = free |> Seq.sumBy Set.count

let rec  findOne anlys known =
    if Map.isEmpty anlys then known else
    let oneFood, oneSet = anlys |> Map.toSeq |> Seq.find (snd >> Set.count >> ((=) 1))
    let oneAllgn = Seq.exactlyOne oneSet
    let anlys = anlys.Remove oneFood
    let anlys = anlys |> Map.map (fun food allgns -> Set.remove oneAllgn allgns)
    findOne anlys ((oneFood, oneAllgn)::known)

let part2 () =
    findOne firstpass [] |> List.sortBy fst |> List.map snd |> String.concat ","


[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0