open Grid

let area = System.IO.File.ReadAllLines("Day03.txt") |> Grid.ofLines

// Better use of unfold inspired by aklefdal:
//     https://github.com/aklefdal/adventofcode-2020/blob/main/aoc-03.fsx
let ntrees (dx, dy) =
    (0, 0)
    |> List.unfold (function
        | _, y when y >= area.Height -> None
        | x, y -> Some ((x, y), ((x + dx) % area.Width, y + dy)))
    |> List.filter (area.Get >> ((=) '#'))
    |> List.length

let part1 = ntrees (3, 1)

let part2 =
    [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2)]
    |> List.map ntrees
    |> List.reduce (*)

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %d"
    part2 |> printfn "Part 2: %d"
    0
