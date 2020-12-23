open System
open Ring

let inline toChars (str: string) = str.ToCharArray()
let inline digitToInt (c: char) = int c - int '0'
let inline intToDigit (d: int) = int '0' + d |> char

let seed =
    System.IO.File.ReadAllLines("Day23.txt").[0]
    |> toChars |> Array.map digitToInt |> Array.toList
let labels size = seed @ [seed.Length + 1 .. size]

let getCup cupArr v =
    let index =
        if v <= seed.Length then List.findIndex ((=) v) seed else (v - 1)
    Array.item index cupArr

let removeAfter cup =
    let rmvd = cup.Next
    let joinTo = rmvd.Next.Next.Next
    cup.Next <- joinTo; joinTo.Prev <- cup; rmvd

let getDestination cupArr cup rmvd =
    let inline next n = if n = 1 then Array.length cupArr else n - 1
    let rec checkDest n = if List.contains n rmvd then checkDest (next n) else n
    let dest = checkDest (Ring.item cup |> next)
    getCup cupArr dest

let rec insertAfter cup rmvd =
    let joinTo = cup.Next
    cup.Next <- rmvd; rmvd.Prev <- cup;
    rmvd.Next.Next.Next <- joinTo; joinTo.Prev <- rmvd.Next.Next

let round cupArr cup =
    let rmvd = removeAfter cup
    let rmvdVals = [rmvd.Item; rmvd.Next.Item; rmvd.Next.Next.Item]
    insertAfter (getDestination cupArr cup rmvdVals) rmvd
    cup |> Ring.next

let play size rounds =
    let start = (labels size) |> Ring.ofList
    let cupArr = start |> Ring.toLinks |> List.toArray
    (start, seq{1 .. rounds}) ||> Seq.fold (fun game _ -> round cupArr game)
    getCup cupArr 1

let part1 () =
    play seed.Length 100 |> Ring.toList |> List.tail
    |> List.map intToDigit  |> List.toArray |> String |> int

let part2 () =
    let cup1 = play 1_000_000 10_000_000
    (int64 cup1.Next.Item) * (int64 cup1.Next.Next.Item)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
