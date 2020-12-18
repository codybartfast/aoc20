open System
open System.Text.RegularExpressions

let inline toCharList (str: string) = str.ToCharArray() |> Array.toList
let lines = System.IO.File.ReadAllLines("Day18.txt")

let rec readNumber digits expr =
    match expr with
    | d::rest when Char.IsNumber d -> readNumber (d::digits) rest
    | _ -> digits |> List.rev |> List.toArray |> String |> int64, expr

let rec applyOperator acc expr advncd =
    let applyToNextNum op expr =
        let v, rest = readNumber [] expr
        applyOperator (op acc v) rest advncd
    match advncd, expr with
    | _, [] -> acc
    | _, ' '::'+'::' '::rest -> applyToNextNum (+) rest
    | false, ' '::'*'::' '::rest -> applyToNextNum (*) rest
    | true, ' '::'*'::' '::rest -> (acc * evalExpr advncd rest)

and evalExpr advncd expr =
    let num, rest = readNumber [] expr
    applyOperator num rest advncd
let evalStr advncd = toCharList >> (evalExpr advncd)

let rec remParens advncd expr =
    let simplified = Regex.Replace(expr, @"\(([\d +*]+)\)", (fun (m: Match) ->
        m.Groups.[1].Value |> (evalStr advncd) |> string))
    if simplified = expr then simplified else remParens advncd simplified

let part1 () = lines |> Array.sumBy ((remParens false) >> (evalStr false))
let part2 () = lines |> Array.sumBy ((remParens true) >> (evalStr true))

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
