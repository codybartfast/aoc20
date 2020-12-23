open System
open System.Text.RegularExpressions
open Ring


let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()
let inline digitToInt (c: char) = int c - int '0'

let input = 
    System.IO.File.ReadAllLines("Day23.txt").[0]
    |> toChars |> Array.map digitToInt |> Array.toList
    
let start () = input |> Ring.ofList
let sorted = input |> List.sortDescending |> Ring.ofList

let remove game =
    let rec remove n rmvd game =
        if n = 0 then game, List.rev rmvd else
        let trgt = Ring.item game
        match Ring.remove game with
        | EmptyRing -> failwith "oops"
        | game -> remove (n - 1) (trgt::rmvd) game
    let game = Ring.next game
    let game, rmvd = remove 3 [] game
    (Ring.prev game, rmvd)

let goDestination game rmvd =
    let rec destination srtd =
        // printfn "Checking for %A " (Ring.item srtd)
        if List.contains (Ring.item srtd) rmvd then 
            destination (Ring.next srtd) 
        else
            Ring.item srtd
    let srtd = sorted |> Ring.find (Ring.item game) |> Ring.next
    Ring.find (destination srtd) game

let rec insert game rmvd =
    (game, rmvd) ||> List.fold (fun g r -> Ring.insert r g)

let move game =    
    let game, rmvd = remove game
    let crnt = Ring.item game
    let game = goDestination game rmvd
    let game = insert game rmvd
    game |> Ring.find crnt |> Ring.next

let rec play n game =
    if n = 0 then game else play (n - 1) (move game)

let final game = 
    game 
    |> Ring.find 1 |> Ring.toList |> List.tail  |> List.map (fun c -> c.ToString() ) |>  String.concat ""

let part1 () =
    play 100 (start ()) |> final


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