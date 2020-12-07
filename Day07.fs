open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day07.txt")
let parse (ln: string) =
    let outer = Regex.Match(ln, @"^(\w+ \w+) bags").Groups.[1].Value
    let inner =
        Regex.Matches(ln, @"(\d+) (\w+ \w+) bag")
        |> Seq.map (fun m -> m.Groups.[1].Value |> int, m.Groups.[2].Value)
        |> Seq.toList
    outer, inner
let rules = lines |> Array.map parse |> Map

let invertMap map vals =
    map
    |> Map.toSeq
    |> Seq.collect (fun (k, v) -> (vals v |> Seq.map (fun v -> v, k)))
    |> Seq.groupBy fst
    |> Seq.map (fun (k, pairs) -> (k, pairs |> Seq.map snd |> Seq.toList))
    |> Map
let invRules = invertMap rules (Seq.map snd)
let bagsContaining bag =
    match invRules.TryFind bag with None -> [] | Some outer -> outer

let rec enumOuter seen (bags: seq<string>) =
    let outer = bags |> Seq.collect (bagsContaining) |> Set
    let newOuter = Set.difference outer seen
    if Set.isEmpty newOuter then
        seen
    else
        enumOuter (Set.union seen newOuter) newOuter

let rec numInside bag =
    rules.[bag] |> List.sumBy (fun (n, bag) -> n + n * numInside bag)

let part1 = enumOuter Set.empty ["shiny gold"] |> Set.count
let part2 = numInside "shiny gold"

[<EntryPoint>]
let main _ = printfn "Part 1: %A" part1; printfn "Part 2: %A" part2; 0
