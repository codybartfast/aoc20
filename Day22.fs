#nowarn "25"

open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let blocks = System.IO.File.ReadAllText("Day22.txt") |> toBlocks
let cards txt = txt |> toLines |> List.tail |> List.map int64
let [| cards1; cards2 |] = blocks |> Array.map cards  

let move (c1::rest1) (c2::rest2) = rest1, rest2 @ [c2; c1]

let simpRound ((c1::_ as cards1), (c2::_ as cards2)) =
    if c1 > c2 then 
        let d2, d1 = move cards2 cards1
        d1, d2
    else 
        move cards1 cards2

let rec simpPlay = function 
    [], deck -> deck | deck, [] -> deck | decks -> simpRound decks |> simpPlay

let rec score =
    List.rev >> List.mapi (fun i card -> (i + 1 |> int64) * card) >> List.sum
    
let part1 () = simpPlay (cards1, cards2) |> score

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