open System
open Ring

let inline toChars (str: string) = str.ToCharArray()
let inline digitToInt (c: char) = int c - int '0'
let inline intToDigit (d: int) = int '0' + d |> char

let seed =
    System.IO.File.ReadAllLines("Day23.txt").[0]
    |> toChars |> Array.map digitToInt |> Array.toList
let labels size = seed @ [seed.Length + 1 .. size]

let getShell shellArr v =
    let index =
        if v <= seed.Length then List.findIndex ((=) v) seed else (v - 1)
    Array.item index shellArr

let removeAfter shell =
    let rmvd = shell.Next
    let joinTo = rmvd.Next.Next.Next
    shell.Next <- joinTo; joinTo.Prev <- shell; rmvd

let getDestination shellArr shell rmvd =
    let inline next n = if n = 1 then Array.length shellArr else n - 1
    let rec checkDest n = if List.contains n rmvd then checkDest (next n) else n
    let dest = checkDest (Ring.item shell |> next)
    getShell shellArr dest

let rec insertAfter shell rmvd =
    let joinTo = shell.Next
    shell.Next <- rmvd; rmvd.Prev <- shell;
    rmvd.Next.Next.Next <- joinTo; joinTo.Prev <- rmvd.Next.Next

let round shellArr shell =
    let rmvd = removeAfter shell
    let rmvdVals = [rmvd.Item; rmvd.Next.Item; rmvd.Next.Next.Item]
    insertAfter (getDestination shellArr shell rmvdVals) rmvd
    shell |> Ring.next

let play size rounds =
    let start = (labels size) |> Ring.ofList
    let shellArr = start |> Ring.toLinks |> List.toArray
    (start, seq{1 .. rounds}) ||> Seq.fold (fun game _ -> round shellArr game)
    getShell shellArr 1

let part1 () =
    play seed.Length 100 |> Ring.toList |> List.tail
    |> List.map intToDigit  |> List.toArray |> String |> int

let part2 () =
    let shell1 = play 1_000_000 10_000_000
    (int64 shell1.Next.Item) * (int64 shell1.Next.Next.Item)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
