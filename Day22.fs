#nowarn "25"

open System
open System.Text.RegularExpressions
open Ring

type Players = Player1 | Player2

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let toDeck = Ring.ofList

let blocks = System.IO.File.ReadAllText("Day22.txt") |> toBlocks
let cards txt = txt |> toLines |> List.tail |> List.map int64
let [| cards1; cards2 |] = blocks |> Array.map cards    

let move cards1 cards2 =
    let card = Ring.item cards1
    Ring.remove cards1, Ring.insert card cards2 |> Ring.advance 1

let simpRound ((Ring cards1), (Ring carsd2)) =
    let c1, c2 = cards1.Item, carsd2.Item
    if c1 = c2 then failwith "oops"
    if c1 > c2 then 
        let d2, d1 = move (Ring carsd2) (Ring cards1)
        d1, d2
    else 
        move (Ring cards1) (Ring carsd2)

let rec simpPlay (deck1, deck2) =
    // printfn "%A - %A" deck1 deck2
    match deck1, deck2 with
    | EmptyRing, _ -> deck2
    | _, EmptyRing -> deck1
    | _ -> simpRound (deck1, deck2) |> simpPlay

let rec score deck =
    match Ring.toList deck with
    | [] -> 0L
    | lst -> 
        lst 
        |> List.rev 
        |> List.mapi (fun i card -> (i + 1 |> int64) * card)
        |> List.sum
    
let part1 () = simpPlay (toDeck cards1, toDeck cards2) |> score

let prity (lst: int64 list)  = lst |> List.map string |> String.concat ", "

let mutable gid = 0
let gameId () = gid <- gid + 1; gid

let rec recRound gid cards1 cards2 gameHist history =
    // printfn "    player1: %A\nplayer2: %A" cards1 cards2

    let (c1::rem1), (c2::rem2) = cards1, cards2
    if c1 > (int64 rem1.Length) ||  c2 > (int64 rem2.Length) then
        if c1 > c2 then Player1, cards1, gameHist else Player2, cards2, gameHist
    else
        // printfn "--->"
        let winner, _, _ = topRound (gameId ()) (List.take (int c1) rem1) (List.take (int c2) rem2) gameHist history
        winner, [], gameHist

and topRound gid cards1 cards2 gameHist history = 
    let key = (cards1, cards2)
    if Set.contains key gameHist then 
        // printfn "history!"
        Player1, cards1, gameHist 
    else
    let gameHist = Set.add key gameHist
        // printfn "\nGame %d\nPlayer1: %A\nPlayer2: %A" gid (prity cards1) (prity cards2)
    let winner, _, gameHist = recRound gid cards1 cards2 gameHist history
    let deck1, deck2 = 
        match winner with
        | Player1 -> 
            let deck2, deck1 = move (toDeck cards2) (toDeck cards1)
            deck1, deck2
        | Player2 -> move (toDeck cards1) (toDeck cards2)
    match deck1, deck2 with
    | EmptyRing, _ -> Player2, (Ring.toList deck2), gameHist
    | _, EmptyRing -> Player1, (Ring.toList deck1), gameHist
    |_ ->    
    topRound gid (Ring.toList deck1) (Ring.toList deck2) gameHist history

let game cards1 cards2 = 
    topRound (gameId ()) cards1 cards2 Set.empty Set.empty

let part2 () = 
    let _, cards, _ = game cards1 cards2
    score (toDeck cards)
    
[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0