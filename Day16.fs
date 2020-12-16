open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let blocks = System.IO.File.ReadAllText("Day16.txt") |> toBlocks

let rules =
    Regex.Split(blocks.[0], @"\r?\n")
    |> Array.map(fun ln ->
        let m = Regex.Match(ln, @"(.*): (\d+)-(\d+) or (\d+)-(\d+)")
        let grpi (n: int) = m.Groups.[n].Value |> int
        let lo, hi, lo2, hi2 = grpi 2, grpi 3, grpi 4, grpi 5
        let validate n = (lo <= n && n <= hi) || (lo2 <= n && n <= hi2)
        (m.Groups.[1].Value, validate))
    |> Array.toList
let nearby =
    Regex.Split(blocks.[2], @"\r?\n").[1..]
    |> Array.map (fun n -> n.Split(',') |> Array.map int)
let ticket = Regex.Split(blocks.[1], @"\r?\n").[1].Split(',') |> Array.map int64

let valid n = rules |> List.exists (fun (_, validate) -> validate n)
let invalid = valid >> not

let part1 () = nearby |> Array.collect id |> Array.filter invalid |> Array.sum

let columns = nearby |> Array.filter (Array.forall valid) |> Array.transpose
let iColumns = columns  |> Array.mapi (fun i (col: int[]) -> (i, col))
let validCol validate (i, col) = col |> Array.forall validate
let allowedCols =
    rules
    |> List.map (fun (name, pred) ->
        name, iColumns |> Array.filter (validCol pred) |> Array.map fst |> Set)
    |> Map
let sortedRules =
    rules |> List.map fst |> List.sortBy (fun rule -> allowedCols.[rule].Count)

let rec matchRulesToCols (matched: Map<string, int>) used rules =
    match rules with
    | [] -> Seq.singleton matched
    | rule::rest ->
        Set.difference allowedCols.[rule] used
        |> Seq.collect (fun col ->
            matchRulesToCols (Map.add rule col matched) (Set.add col used) rest)

let part2 () =
    let fieldMap = matchRulesToCols Map.empty Set.empty sortedRules |> Seq.head
    rules.[0..5]
    |> List.map (fst >> (fun rule -> ticket.[ fieldMap.[rule] ]))
    |> List.reduce (*)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
