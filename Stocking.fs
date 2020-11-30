module Stocking

open System

let nl = Environment.NewLine

// strings, chars, hex
let inline toChars (str: string) = str.ToCharArray()
let inline encode (str: string) = System.Text.Encoding.ASCII.GetBytes(str)
let inline toHex (bs: byte[]) = Convert.ToHexString(bs).ToLower()
let inline digitToInt (c: char) = int c - int '0'

let rec pairCombos = function
    // aoc18:02
    | [] | [_] -> []
    | head::tail ->
        List.map (fun e -> (head, e)) tail @ pairCombos tail

let rec permutations list =
    // aoc15:13
    let rec insAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insAlong head) (permutations tail)

let adjcrds (x, y) width height =
    // aoc16:22
    // [(x, y + 1); (x + 1, y + 1); (x + 1, y); (x + 1, y - 1); 
    //  (x, y - 1); (x - 1, y - 1); (x - 1, y); (x - 1, y + 1)]
    [(x, y + 1); (x + 1, y); (x, y - 1); (x - 1, y)]
    |> List.filter (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height

let mapToString (map:Map<(int * int), int64>) =
    // aoc18:20
    let valueAsChar = function
        | None | Some 0L -> ' '
        | Some 1L -> '█'
        | u -> failwithf "Unexpected value: %O" u

    let coords = map |> Map.toList |> List.map fst
    let minX = coords |> (List.map fst >> List.min)
    let maxX = coords |> (List.map fst >> List.max)
    let minY = coords |> (List.map snd >> List.min)
    let maxY = coords |> (List.map snd >> List.max)
    let (width, height) = (1 + maxX-minX, 1 + maxY-minY)
    let (xShift, yShift) = ((+) minX), ((+) minY)

    Array.init (height)  (fun  y ->
        Array.init (width) (fun x ->
            map.TryFind (xShift x, yShift y) |> valueAsChar))
    |> Array.map String
    |> String.concat nl

