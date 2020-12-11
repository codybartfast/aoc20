open System
open System.Text.RegularExpressions
open Grid

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let area = System.IO.File.ReadAllLines("Day11.txt") |> Grid.ofLines
type Area = Grid<char>

let seat1 (area: Area) x y =
    if area.[x, y] = '.' then '.' else
    let adj = area.Adjacent (x,y)
    match adj |> List.map snd |> List.filter ((=) '#') |> List.length with
    | 0 -> '#'
    | 1 | 2 | 3 -> area.Get (x, y)
    | _ -> 'L'

let round1 (area: Area) = area.Transform seat1
    
let part1 () =
    area
    |> Seq.unfold (fun area ->
        let nxt = round1 area
        if nxt.ToString() = area.ToString() then None else
        Some (nxt, nxt))
    |> Seq.last
    |> Grid.count ((=) '#')

let visSeat (area: Area) (x, y) (dx, dy) =
    let inbounds (x, y) = (x >= 0 && y >= 0 && x < area.Width && y < area.Height)
    // let (++) (x, y) (x', y') = (x + x', y + y')
    let view = (x, y) |> List.unfold (fun (x, y) ->
        if not (inbounds (x, y)) then None else
        let nxt = x + dx, y + dy
        Some ((x, y), nxt))
    let sts = view.Tail |> List.map area.Get |> List.skipWhile ((=) '.')
    match sts with
    | [] -> '.'
    | h::t -> h
    
let adjacent (area: Area) (x, y) =
    [(0, -1); (1, -1); (1, 0); (1, 1);
     (0, 1); (-1, 1); (-1, 0); (-1, -1)]
    |> List.map (visSeat area (x, y))

let seat2 (area: Area) x y =
    if area.[x, y] = '.' then '.' else
    let adj = adjacent area (x, y)
    match adj |> List.filter ((=) '#') |> List.length with
    | 0 -> '#'
    | 1 | 2 | 3 | 4 -> area.Get (x, y)
    | _ -> 'L'

let round2 (area: Area) = area.Transform seat2


let part2 () =
//     let full = area |> (round2 >> id)
//     adjacent full (0, 0)
    area
    |> Seq.unfold (fun area ->
        let nxt = round2 area
        if nxt.ToString() = area.ToString() then None else
        Some (nxt, nxt))
    |> Seq.last
    |> Grid.count ((=) '#')

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
