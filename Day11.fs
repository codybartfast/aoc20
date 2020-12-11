open Grid

type Area = Grid<char>

let area = System.IO.File.ReadAllLines("Day11.txt") |> Grid.ofLines

let (++) (x, y) (x', y') = (x + x', y + y')

let considerAdjacent (area: Area) seat v =
    if area.Get seat = '.' then '.' else
    area.Adjacent seat |> List.filter (snd >> (=) '#') |> List.length
    |> function
        | 0 -> '#'
        | 1 | 2 | 3 -> v
        | _ -> 'L'

let shuffle consider (area: Area) =
    area
    |> Seq.unfold (fun area ->
        let nxt = area.Transform consider
        if nxt = area then None else Some (nxt, nxt))
    |> Seq.last
    |> Grid.count ((=) '#')

let part1 () = shuffle considerAdjacent area

let visible (area: Area) seat dir =
    seat |> Seq.unfold (fun seat ->
        let nxt = seat ++ dir
        if area.InRange nxt then Some (nxt, nxt) else None)
    |> Seq.map area.Get
    |> Seq.skipWhile ((=) '.')
    |> Seq.tryHead
    |> function None -> '.' | Some h -> h

let considerVisible (area: Area) seat v =
    if area.Get seat = '.' then '.' else
    [(0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1)]
    |> List.map (visible area seat) |> List.filter ((=) '#') |> List.length
    |> function
        | 0 -> '#'
        | 1 | 2 | 3 | 4 -> v
        | _ -> 'L'

let part2 () = shuffle considerVisible area

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
