open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day10.txt")
let parse (ln: string) = int ln
let input = lines |> Array.map parse |> Array.toList

let jolts = 0 :: input @ [ (List.max input) + 3 ] |> List.sort

let part1 () =
    let diffs = 
        jolts |> List.pairwise  |> List.map (fun (a, b) -> b - a)
    let ones = diffs |> List.filter ((=) 1) |> List.length
    let threes = diffs |> List.filter ((=) 3) |> List.length
    ones * threes


// let rec count j jolts = seq{
//     match jolts with 
//     | [] -> yield 1 
//     | _ -> 
//         let poss = jolts |> List.takeWhile (fun p -> j < p && p <= j + 3)
//         yield! poss |> Seq.collect (fun p -> count p (jolts |> List.skipWhile (fun x -> x <= p))) }

let add counts j =
    printfn "%A  - %A" counts j
    let cnt = 
        counts 
        |> List.takeWhile (fst >> ((>=) (j + 3)))
        |> List.sumBy (snd)
    (j, cnt)::counts

let part2 () = 
    let rjolts = jolts |> List.rev
    ([(rjolts.Head, 1L)], rjolts.Tail)
    ||> List.fold add
    |> List.head




//count 0 jolts.Tail |> Seq.length
    

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
