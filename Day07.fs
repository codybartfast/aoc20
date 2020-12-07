open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day07.txt") |> Array.toList
let parse (ln: string) =
    let outer = Regex.Match(ln, @"^(\w+ \w+) bags").Groups.[1].Value
    let inner =
        Regex.Matches(ln, @"(\d+) (\w+ \w+) bag")
        |> Seq.map (fun m -> m.Groups.[1].Value |> int, m.Groups.[2].Value)
        |> Seq.toList
    outer, inner
let rules = lines |> List.map parse |> Map

let rulesContaining bag =
    rules
    |> Map.toSeq |> Seq.toList
    |> List.filter (snd >> List.exists (snd >> ((=) bag)))

let rec enumOuter seen bags =
    let newBags =
        bags
        |> List.collect (rulesContaining >> (List.map fst))
        |> List.filter (fun bag -> not (Set.contains bag seen))
    match newBags with
    | [] -> seen
    | _ -> enumOuter (Set.union seen (Set newBags)) newBags

let rec reqInner bag =
    rules.[bag]
    |> List.sumBy (fun (n, bag) -> n + n * reqInner bag)

let part1 = enumOuter Set.empty ["shiny gold"] |> Set.count
let part2 = reqInner "shiny gold"

[<EntryPoint>]
let main _ = printfn "Part 1: %A" part1; printfn "Part 2: %A" part2; 0
