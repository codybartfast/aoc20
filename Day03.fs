open Grid

let area = System.IO.File.ReadAllLines("Day03.txt") |> Grid.ofLines

let ntrees (dx, dy) =
    (0, 0)
    |> Seq.unfold (fun (x, y) -> Some ((x, y), (x + dx, y + dy)))
    |> Seq.takeWhile (fun (_, y) -> y < area.Height)
    |> Seq.map (fun (x, y) -> area.[x % area.Width, y])
    |> Seq.filter ((=) '#')
    |> Seq.length

let part1 = ntrees (1, 3)

let part2 =
    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map (ntrees >> int64)
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %d"
    part2 |> printfn "Part 2: %d"
    0
