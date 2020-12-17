open System
open System.Text.RegularExpressions

type Coord = int * int * int

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")

let lines = System.IO.File.ReadAllLines("Day17.txt")
let inline toChars (str: string) = str.ToCharArray()
let start =
    lines |> Array.map toChars |> Array.toList
    |> List.mapi (fun y row -> row |> Array.mapi (fun x cube -> ((x, y, 0), cube)))
    |> Seq.collect id
    |> Map
let (++) (x, y, z) (x', y', z') = (x + x', y + y', z + z')

let adjacentCoords =
    [ (-1, -1, -1); ( 0, -1, -1); (1, -1, -1);  (-1,  0, -1); ( 0,  0, -1); (1,  0, -1); (-1,  1, -1); ( 0,  1, -1); (1,  1, -1);
      (-1, -1,  0); ( 0, -1,  0); (1, -1,  0);  (-1,  0,  0);               (1,  0,  0); (-1,  1,  0); ( 0,  1,  0); (1,  1,  0);
      (-1, -1,  1); ( 0, -1,  1); (1, -1,  1);  (-1,  0,  1); ( 0,  0,  1); (1,  0,  1); (-1,  1,  1); ( 0,  1,  1); (1,  1,  1); ]

let adjacent coord = adjacentCoords |> List.map ((++) coord)
let nhood coord =  coord::(adjacentCoords |> List.map ((++) coord))

let region space = space |> Map.toSeq |> Seq.collect (fst >> nhood) |> Set

let next (space: Map<Coord, char>) coord =
    let isActive coord = 
        match Map.tryFind coord space with | Some '#' -> true | _ -> false
    let count = adjacent coord  |> List.filter isActive |> List.length
    match isActive coord, count with
    | true, 2 | true, 3 | false, 3 -> coord, '#'
    | _ -> coord, '.'
let generation space = region space |> Seq.map (next space) |> Map

let part1 () =
    start 
    |> generation
    |> generation 
    |> generation
    |> generation 
    |> generation
    |> generation 
    |> Map.toSeq |> Seq.filter (snd >> ((=) '#')) |> Seq.length

let part2 () =
    "?"

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
