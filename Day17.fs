open System
open System.Text.RegularExpressions

type Coord = int * int * int * int

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")

let lines = System.IO.File.ReadAllLines("Day17.txt")
let inline toChars (str: string) = str.ToCharArray()
let start =
    lines |> Array.map toChars |> Array.toList
    |> List.mapi (fun y row -> row |> Array.mapi (fun x cube -> ((x, y, 0, 0), cube)))
    |> Seq.collect id
    |> Seq.filter (snd >> ((=) '#'))
    |> Seq.map fst
    |> Set

let (++) (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')

let adjacentCoords =
  [ (-1, -1, -1, -1); ( 0, -1, -1, -1); (1, -1, -1, -1);  (-1,  0, -1, -1); ( 0,  0, -1, -1); (1,  0, -1, -1); (-1,  1, -1, -1); ( 0,  1, -1, -1); (1,  1, -1, -1);
    (-1, -1,  0, -1); ( 0, -1,  0, -1); (1, -1,  0, -1);  (-1,  0,  0, -1); ( 0,  0,  0, -1); (1,  0,  0, -1); (-1,  1,  0, -1); ( 0,  1,  0, -1); (1,  1,  0, -1);
    (-1, -1,  1, -1); ( 0, -1,  1, -1); (1, -1,  1, -1);  (-1,  0,  1, -1); ( 0,  0,  1, -1); (1,  0,  1, -1); (-1,  1,  1, -1); ( 0,  1,  1, -1); (1,  1,  1, -1);
  
    (-1, -1, -1,  0); ( 0, -1, -1,  0); (1, -1, -1,  0);  (-1,  0, -1,  0); ( 0,  0, -1,  0); (1,  0, -1,  0); (-1,  1, -1,  0); ( 0,  1, -1,  0); (1,  1, -1,  0);
    (-1, -1,  0,  0); ( 0, -1,  0,  0); (1, -1,  0,  0);  (-1,  0,  0,  0);                   (1,  0,  0,  0); (-1,  1,  0,  0); ( 0,  1,  0,  0); (1,  1,  0,  0);
    (-1, -1,  1,  0); ( 0, -1,  1,  0); (1, -1,  1,  0);  (-1,  0,  1,  0); ( 0,  0,  1,  0); (1,  0,  1,  0); (-1,  1,  1,  0); ( 0,  1,  1,  0); (1,  1,  1,  0);
  
    (-1, -1, -1,  1); ( 0, -1, -1,  1); (1, -1, -1,  1);  (-1,  0, -1,  1); ( 0,  0, -1,  1); (1,  0, -1,  1); (-1,  1, -1,  1); ( 0,  1, -1,  1); (1,  1, -1,  1);
    (-1, -1,  0,  1); ( 0, -1,  0,  1); (1, -1,  0,  1);  (-1,  0,  0,  1); ( 0,  0,  0,  1); (1,  0,  0,  1); (-1,  1,  0,  1); ( 0,  1,  0,  1); (1,  1,  0,  1);
    (-1, -1,  1,  1); ( 0, -1,  1,  1); (1, -1,  1,  1);  (-1,  0,  1,  1); ( 0,  0,  1,  1); (1,  0,  1,  1); (-1,  1,  1,  1); ( 0,  1,  1,  1); (1,  1,  1,  1) ]

let adjacent coord = adjacentCoords |> List.map ((++) coord)
let nhood coord =  coord::(adjacentCoords |> List.map ((++) coord))

let region space = space |> Seq.collect nhood |> Seq.distinct

let next (space: Set<Coord>) coord =
    let isActive coord =  space.Contains coord 
    let count = adjacent coord  |> List.filter isActive |> List.length
    match isActive coord, count with
    | true, 2 | true, 3 | false, 3 -> Some coord
    | _ -> None
    
let generation space = space |> region |> Seq.choose (next space) |> Set

let part1 () =
    start 
    |> generation
    |> generation 
    |> generation
    |> generation 
    |> generation
    |> generation 
    |> Set.count

let part2 () =
    "?"

[<EntryPoint>]
let main _ = 
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start ()
    printfn "Part 1: %A" (part1 ());
    printfn "Part 2: %A" (part2 ());
    sw.Stop ()
    printfn "%f" sw.Elapsed.TotalSeconds
    0