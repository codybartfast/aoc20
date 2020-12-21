open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day21.txt")
let parse (ln: string) =
    let m = Regex.Match(ln, @"((?<i>\w+) )+\(contains ((?<a>\w+)[, ]*)+")
    let ingrds = m.Groups.["i"].Captures |> Seq.map (fun c -> c.Value) |> Set
    let allgns = m.Groups.["a"].Captures |> Seq.map (fun c -> c.Value) |> Set
    ingrds, allgns
let foods = lines |> Array.map parse |> Array.toList

let allIngrds = foods |> List.map fst |> Set.unionMany
let allAllgns = foods |> List.map snd |> Set.unionMany
let baseAnalysis = allAllgns |> Seq.map (fun a -> (a, allIngrds)) |> Map

let analyse info (ingrds, allgns) =
    (info, allgns)
    ||> Seq.fold( fun info allgn ->
        Map.add allgn (Set.intersect info.[allgn] ingrds) info)

let analysis = (baseAnalysis, foods) ||> List.fold analyse
let suspect = analysis |> Map.toSeq |> Seq.map snd |> Set.unionMany
let allgnFree =
    foods |> List.map (fst >> (fun ingrds -> Set.difference ingrds suspect))

let part1 () = allgnFree |> Seq.sumBy Set.count

let rec identify identified analysis =
    if Map.isEmpty analysis then identified else
    let food, allgns =
        analysis |> Map.toSeq |> Seq.find (snd >> Set.count >> ((=) 1))
    let allgn = Seq.exactlyOne allgns
    analysis.Remove food
    |> Map.map (fun _ allgns -> Set.remove allgn allgns)
    |> identify ((food, allgn)::identified)

let part2 () =
    identify [] analysis |> List.sortBy fst |> List.map snd |> String.concat ","

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
