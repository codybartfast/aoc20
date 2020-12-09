let input = Array.map int64 (System.IO.File.ReadAllLines("Day09.txt"))
let preamble, cipher = input |> Array.splitAt 25

let pairs len =
    [| for i in 0..(len - 2) do for j in (i + 1)..(len - 1) do yield i, j |]

let isInvalid preamble n =
    pairs (Array.length preamble)
    |> Array.map (fun (a, b) -> preamble.[a] + preamble.[b])
    |> Array.contains n
    |> function false -> Some n | _ -> None

let findInvalid =
    (preamble, cipher)
    ||> Array.mapFold(fun preamble n ->
        (isInvalid preamble n), (Array.append preamble.[1..] [|n|]))
    |> fst

let weakness target =
    pairs input.Length
    |> Seq.map (fun (i, j) -> input.[i..j])
    |> Seq.find (Array.sum >> ((=) target))
    |> fun contig -> Array.min contig + Array.max contig

let part1 = findInvalid  |> Array.pick id
let part2 = weakness part1

[<EntryPoint>]
let main _ = printfn "Part 1: %d" part1; printfn "Part 2: %d" part2; 0
