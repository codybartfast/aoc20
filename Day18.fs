open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day18.txt")

let rec readNumber expr =
    let digits = Regex.Match(expr, @"^\d+").Value
    int64 digits, expr.[digits.Length..]

let rec applyOperator acc expr advncd =
    let applyToNextNum op expr =
        let v, rest = readNumber expr
        applyOperator (op acc v) rest advncd
    let opStr, rest = expr.[1..1], expr.[3..]
    match advncd, opStr with
    | _, "" -> acc
    | _, "+" -> applyToNextNum (+) rest
    | false, "*" -> applyToNextNum (*) rest
    | true, "*" -> (acc * evalSimple advncd rest)

and evalSimple advncd expr =
    let num, rest = readNumber expr
    applyOperator num rest advncd

let rec removeParens advncd expr =
    let simplified = Regex.Replace(expr, @"\(([\d +*]+)\)", (fun (m: Match) ->
        m.Groups.[1].Value |> (evalSimple advncd) |> string))
    if simplified = expr then simplified else removeParens advncd simplified

let eval advanced =  (removeParens advanced) >> evalSimple (advanced)

let part1 () = lines |> Array.sumBy (eval false)
let part2 () = lines |> Array.sumBy (eval true)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
