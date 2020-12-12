type Instr =
    | Move of (int * int)
    | Turn of (int * int -> int * int)
    | Forward of int
type MoveWhat = Ship | Waypoint

let (++) (x, y) (x', y') = (x + x', y + y')
let scale n (x, y) = (n * x, n * y)
let dist ((x, y), _) = abs x + abs y

let lines = System.IO.File.ReadAllLines("Day12.txt")
let parse (ln: string) =
    match ln.[0], int ln.[1..] with
    | 'N', n -> Move (0, -n)
    | 'E', n -> Move (n, 0)
    | 'S', n -> Move (0, n)
    | 'W', n -> Move (-n, 0)
    | 'L', 90 | 'R', 270 -> Turn (fun (x, y) -> (y, -x))
    | 'R', 90 | 'L', 270 -> Turn (fun (x, y) -> (-y, x))
    | _, 180 -> Turn (fun (x, y) -> (-x, -y))
    | 'F', n -> Forward n
let insts = lines |> Array.map parse |> Array.toList

let sail moveWhat (shp, waypoint) inst =
    match moveWhat, inst with
    | Ship, Move v -> shp ++ v, waypoint
    | Waypoint, Move v -> shp, waypoint ++ v
    | _, Turn turn -> shp, turn waypoint
    | _, Forward n -> (shp ++ (scale n waypoint)), waypoint

let part1 () = (((0, 0), (1, 0)), insts) ||> List.fold (sail Ship) |> dist
let part2 () = (((0, 0), (10, -1)), insts) ||> List.fold (sail Waypoint) |> dist

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
