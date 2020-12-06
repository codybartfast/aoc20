open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toChars (str: string) = str.ToCharArray()
let groups =
    System.IO.File.ReadAllText("Day06.txt")
    |> toBlocks
    |> Array.map (fun lns -> Regex.Split(lns, @"\r?\n") |> (Array.map toChars))

let part1 =
    groups |> Array.sumBy (Array.collect id >> Array.distinct >> Array.length)
let part2 =
    groups |> Array.sumBy (Array.map Set >> Set.intersectMany >> Set.count)

[<EntryPoint>]
let main _ = printfn "Part 1: %d" part1; printfn "Part 2: %d" part2; 0
