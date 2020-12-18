open System
open System.Text.RegularExpressions
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day18.txt")

let rec readNumber digits expr  =
    match expr with
    | d::rest when Char.IsNumber d -> readNumber  (d::digits) rest
    | _ -> digits |> List.rev |> List.toArray |> String |> int64, expr

let rec applyOperator acc expr = 
    match expr with
    | [] -> acc
    | ' '::'+'::' '::rest ->
        let v, rest = readNumber [] rest
        applyOperator (acc + v) rest
    | ' '::'*'::' '::rest -> (acc * (rest |> List.toArray |> String |> evalStr))
    
and evalStr ln =
    let v, rest = ln |> toChars |> Array.toList |> (readNumber [])
    applyOperator v rest

let rec depParens (str: string) =
    let simp = Regex.Replace(str, @"\(([\d +*]+)\)", (fun (m: Match) -> 
        m.Groups.[1].Value |> evalStr |> string))
    if simp = str then str else depParens simp


let part1 () = 
    lines |> Array.sumBy (depParens >> evalStr) 

    

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