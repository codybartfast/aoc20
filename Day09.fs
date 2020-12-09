let rec pairCombos = function
    | [] | [_] -> []
    | head::tail -> List.map (fun e -> (head, e)) tail @ pairCombos tail

let input =
    System.IO.File.ReadAllLines("Day09.txt") |> Array.map int64 |> Array.toList
let preamble, cipher = input |> List.splitAt 25

let invalid pool n =
    pairCombos pool
    |> List.map (fun (a, b) -> a + b)
    |> (List.contains n)
    |> (function false -> Some n | _ -> None)

let findInvalid preamble cipher =
    (preamble, cipher)
    ||> List.mapFold(fun pool n -> (invalid pool n), List.tail pool @ [n])
    |> fst

let weakness target cipher =
    let nums = Array.ofList cipher
    let len = nums.Length
    seq { for i in 0 .. len - 2 do for j in i + 1 .. len - 1 do yield (i, j) }
    |> Seq.map (fun (i, j) -> Array.sub nums i (j - i))
    |> Seq.find (Array.reduce (+) >> ((=) target))
    |> (fun contig -> Array.min contig + Array.max contig)

let part1 = findInvalid preamble cipher |> List.pick id
let part2 () = weakness part1 (preamble@cipher)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1); printfn "Part 2: %d" (part2 ()); 0
