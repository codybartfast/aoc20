open System

let lines = System.IO.File.ReadAllLines("Day05.txt") |> Array.toList
let seat (ln: string) =
    let toInt (chrs: char[]) = Convert.ToInt32(String chrs, 2)
    let binChars =
        ln.ToCharArray() |> Array.map (function 'F' | 'L' -> '0' | _ -> '1')
    let row = binChars |> Array.take 7 |> toInt
    let col = binChars |> Array.skip 7 |> toInt
    (row, col)
let seats = lines |> List.map seat

let seatId (r, c) = r * 8 + c

let part1 = seats |> List.map seatId |> List.max

let findTheGap (row, seats) =
    let adjacent (c, c') = c + 1 = c'
    let colPairs = seats |> List.map snd |> List.sort |> List.pairwise
    match colPairs |> List.forall adjacent with
    | true -> None
    | false ->
        colPairs
        |> List.takeWhile adjacent
        |> List.last
        |> fun (_, c) -> Some (row, c + 1)

let part2 = seats |> List.groupBy fst |> List.pick findTheGap |> seatId

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
