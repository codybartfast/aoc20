open System.Text.RegularExpressions

type Players = Player1 | Player2

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList

let input =
    System.IO.File.ReadAllText("Day22.txt") |> toBlocks
    |> Array.map (toLines >> List.tail >> List.map int)
let decks = (input.[0], input.[1])

let takeCards winner (deck1, deck2) =
    let move (c1::rest1) (c2::rest2) = rest1, rest2 @ [c2; c1]
    match winner with
    | Player1 ->
        let cards2, cards1 = move deck2 deck1
        cards1, cards2
    | Player2 -> move deck1 deck2

let simpRound (((c1::_), (c2::_)) as decks) =
    takeCards (if c1 > c2 then Player1 else Player2) decks

let rec simpPlay = function
    | [], deck | deck, [] -> deck
    | decks -> simpRound decks |> simpPlay

let rec score = List.rev >> List.mapi (fun i card -> (i + 1) * card) >> List.sum

let part1 () = simpPlay decks |> score

let rec round ((c1::rest1 as deck1), (c2::rest2) as decks) hist =
    if Set.contains decks hist then Player1, deck1 else
    let hist = Set.add decks hist
    let winner =
        if c1 > rest1.Length || c2 > rest2.Length then
            if c1 > c2 then Player1 else Player2
        else
            round (List.take c1 rest1, List.take c2 rest2) hist |> fst
    match takeCards winner decks with
    | deck1, [] -> Player1, deck1
    | [], deck2 -> Player2, deck2
    | decks -> round decks hist

let part2 () = round decks Set.empty |> snd |> score

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
