open System
open System.Text.RegularExpressions

let inline toChars (str: string) = str.ToCharArray()
let (++) (x, y) (x', y') = (x + x', y + y')
let (--) (x, y) (x', y') = (x - x', y - y')

let lines = System.IO.File.ReadAllLines("Day12.txt")
let parse (ln: string) = ln.[0], int ln.[1..]
let instrs = lines |> Array.map parse |> Array.toList
let start = (((0, 0), 'E'), (10, -1))

let vects = Map [('N', (0, -1)); ('E', (1, 0)); ('S', (0, 1)); ('W', (-1, 0))]

let move (crd, dir) bearing n =
    let (dx, dy) = vects.[bearing]
    let v = (dx * n), (dy * n)
    crd ++ v, dir

let forward (crd, dir) n =
    let (dx, dy) = vects.[dir]
    crd ++ (dx * n, dy * n), dir
    
let turn (crd, dir) turn n =
    let reverse = function 'N' -> 'S' | 'E' -> 'W' | 'S' -> 'N' | 'W' -> 'E'
    let right = function 'N' -> 'E' | 'E' -> 'S' | 'S' -> 'W' | 'W' -> 'N'
    let left = right >> reverse
    let turn =
        match turn, n with
        | _, 180 -> reverse
        | 'L', 90 | 'R', 270 -> left
        | 'R', 90 | 'L', 270 -> right
    (crd, turn dir)


let exec pos (act, qty) =
    match act with
    | 'F' -> forward pos qty
    | 'L' | 'R' -> turn pos act qty
    | _ -> move pos act qty

let dist ((x, y), _) = abs x + abs y

let part1 () = (fst start, instrs) ||> List.fold exec |> dist

let forwardW ((shp, dir), (wx, wy)) n =
    let wp = (wx * n, wy * n)
    ((shp ++ wp), dir), (wx, wy)

let moveW ((shp, dir), wp) bearing n =
    let (dx, dy) = vects.[bearing]
    let v = (dx * n), (dy * n)
    ((shp, dir), wp ++ v)

let turnW ((shp, dir), wp) turn n =
    let reverse = function 'N' -> 'S' | 'E' -> 'W' | 'S' -> 'N' | 'W' -> 'E'
    let right = function 'N' -> 'E' | 'E' -> 'S' | 'S' -> 'W' | 'W' -> 'N'
    let left = right >> reverse
    let turnD =
        match turn, n with
        | _, 180 -> reverse
        | 'L', 90 | 'R', 270 -> left
        | 'R', 90 | 'L', 270 -> right
    let dir = turnD dir

    let reverseV (x, y) = (-x , -y)
    let rightV (x, y) = (-y, x)
    let leftV = rightV >> reverseV
    let turnV =
        match turn, n with
        | _, 180 -> reverseV
        | 'L', 90 | 'R', 270 -> leftV
        | 'R', 90 | 'L', 270 -> rightV
    let wp = turnV wp

    ((shp, dir), wp)

let execW pos (act, qty) =
    match act with
    | 'F' -> forwardW pos qty
    | 'L' | 'R' -> turnW pos act qty
    | _ -> moveW pos act qty

let part2 () = (start, instrs) ||> List.fold execW |> fst |> dist

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
