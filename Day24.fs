open System.Text.RegularExpressions

let (++) (x, y) (x', y') = (x + x', y + y')
let repeat fn n = List.init n (fun _ -> fn) |> List.reduce (>>)

let e, se, sw, w, nw, ne = (2, 0), (1 ,1),(-1, 1), (-2, 0), (-1, -1), (1, -1)
let dirs = [e; se; sw; w; nw; ne]

let lines = System.IO.File.ReadAllLines("Day24.txt")
let parse (ln: string) =
    let cs = Regex.Matches(ln, @"(e|se|sw|w|nw|ne)")
    cs |> Seq.map (fun c -> c.Value) |> Seq.toList
    |> List.map (function
        "e" -> e | "se" -> se | "sw" -> sw | "w" -> w | "nw" -> nw | "ne" -> ne)
let pathEnd path = ((0, 0), path) ||> List.fold (++)
let paths = lines |> Array.map parse |> Array.toList

let flip floor crd =
    let tile = match Map.tryFind crd floor with Some true -> false | _ -> true
    floor.Add (crd, tile)
let flipPath floor path = flip floor (pathEnd path)
let floor0 =  (Map.empty, paths) ||> List.fold flipPath
let countBlack = Map.toSeq >> Seq.filter snd >> Seq.length

let part1 () = floor0 |> countBlack

let isBlack floor crd =
    match Map.tryFind crd floor with Some true -> true | _ -> false

let nextCoords =
    Map.toSeq
    >> Seq.collect (fun (crd, _) -> dirs |> Seq.map ((++) crd))
    >> Seq.distinct

let nextTile floor crd =
    let blackNeighbours =
        dirs |> List.filter ((++) crd >> isBlack floor) |> List.length
    crd, match isBlack floor crd, blackNeighbours with
         | true, 1| true, 2 | false, 2 -> true | _ -> false

let nextFloor floor = floor |> nextCoords |> Seq.map (nextTile floor) |> Map

let part2 () = floor0 |> repeat nextFloor 100 |> countBlack

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
