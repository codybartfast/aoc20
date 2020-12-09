let rec pairCombos = function
    | [] | [_] -> []
    | head::tail -> List.map (fun e -> (head, e)) tail @ pairCombos tail

let input = Array.map int64 (System.IO.File.ReadAllLines("Day09.txt"))
let preamble, cipher = input |> Array.splitAt 25

let isInvalid pool n =
    pairCombos pool
    |> List.map (fun (a, b) -> a + b)
    |> List.contains n
    |> function false -> Some n | _ -> None

let findInvalid preamble cipher =
    (preamble |> Array.toList, cipher)
    ||> Array.mapFold(fun pool n -> isInvalid pool n, List.tail pool @ [n])
    |> fst

let weakness target cipher =
    let len = Array.length cipher
    seq { for i in 0 .. len - 2 do for j in i + 1 .. len - 1 do yield i, j }
    |> Seq.map (fun (i, j) -> Array.sub cipher i (j - i))
    |> Seq.find (Array.reduce (+) >> ((=) target))
    |> fun contig -> Array.min contig + Array.max contig

let part1 = findInvalid preamble cipher |> Array.pick id
let part2 = weakness part1 input

[<EntryPoint>]
let main _ = printfn "Part 1: %d" part1; printfn "Part 2: %d" part2; 0
