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
let inputLen = input.Length

let start () = input |> Ring.ofList
let sorted = input |> List.sortDescending |> Ring.ofList
let ringArr = start () |> Ring.toLinks |> List.toArray

let getIndex n =
    if n <= inputLen then List.findIndex ((=) n) input else n

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

let gotoValue value =
    Ring (ringArr.[getIndex value])

let goDestination max game rmvd =
    let rec destination n =
        let n = if n = 0 then max else n
        if List.contains n rmvd then
            destination (n - 1)
        else
            n
    let dest = destination (Ring.item game - 1)
    gotoValue dest

let rec insert game rmvd =
    (game, rmvd) ||> List.fold (fun g r -> Ring.insert r g)

let move size game =
    printfn "move game length: %A" (game |> Ring.toList |> List.length)
    let game, rmvd = remove game
    let crnt = Ring.item game
    let game = goDestination size game rmvd
    let game = insert game rmvd
    (gotoValue crnt) |> Ring.next

let rec play size n game =
    // if n % 1 = 0 then printfn "remaining: %d" n
    if n = 0 then game else play size (n - 1) (move size game)

let final game =
    game
    |> Ring.find 1 |> Ring.toList |> List.tail  |> List.map (fun c -> c.ToString() ) |>  String.concat ""

let part1 () =
    printfn "started"
    play input.Length 100 (start ()) |> final

let bigstart =
    let fillStart = List.max input + 1
    let fillEnd = fillStart + (1_000_000 - input.Length) - 1
    input @ [fillStart .. fillEnd] |> Ring.ofList

let part2 () =
    "" //gotoValue 9

[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0