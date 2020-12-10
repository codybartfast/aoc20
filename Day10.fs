let lines = System.IO.File.ReadAllLines("Day10.txt")
let input = lines |> Array.map int |> Array.toList
let jolts = 0 :: input @ [ (List.max input) + 3 ] |> List.sort

let part1 () =
    let diffs = jolts |> List.pairwise  |> List.map (fun (a, b) -> b - a)
    let n1s = diffs |> List.filter ((=) 1) |> List.length
    let n3s = diffs |> List.filter ((=) 3) |> List.length
    n1s * n3s

let countVariationsThatUse counts jolt =
    let count =
        counts
        |> List.takeWhile (fst >> ((<=) (jolt - 3)))
        |> List.sumBy (snd)
    (jolt, count)::counts

let part2 () =
    ([(jolts.Head, 1L)], jolts.Tail)
    ||> List.fold countVariationsThatUse
    |> List.head |> snd

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
