open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day18.txt")

let rec readNumber expr =
    let digits = Regex.Match(expr, @"^\d+").Value
    int64 digits, expr.[digits.Length..]

let rec applyOperator advncd acc expr =
    let applyToNextNum op expr =
        let v, rest = readNumber expr
        applyOperator advncd (op acc v) rest
    let operator, rest = expr.[1..1], expr.[3..]
    match advncd, operator with
    | _, "" -> acc
    | _, "+" -> applyToNextNum (+) rest
    | false, "*" -> applyToNextNum (*) rest
    | true, "*" -> (acc * eval advncd rest)
and eval advncd = readNumber >> fun (num, rest) -> applyOperator advncd num rest

let rec removeParens advncd expr =
    let simplified = Regex.Replace(expr, @"\(([^()]+)\)", (fun (m: Match) ->
        m.Groups.[1].Value |> eval advncd |> string))
    if simplified = expr then simplified else removeParens advncd simplified

let homework advncd =  removeParens advncd >> eval advncd

let part1 () = lines |> Array.sumBy (homework false)
let part2 () = lines |> Array.sumBy (homework true)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
