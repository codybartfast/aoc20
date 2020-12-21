module Stocking

open System
open System.Text.RegularExpressions

let nl = Environment.NewLine

// parsing
let captures (m: Match) (cpt: string) =
    m.Groups.[cpt].Captures |> Seq.map (fun c -> c.Value) |> Seq.toList
let parensPtn = @"\((?>\((?<DEPTH>)|\)(?<-DEPTH>)|[^()]+)*\)(?(DEPTH)(?!))"

// strings, chars, hex
let inline toChars (str: string) = str.ToCharArray()
let inline encode (str: string) = System.Text.Encoding.ASCII.GetBytes(str)
let inline toHex (bs: byte[]) = Convert.ToHexString(bs).ToLower()
let inline digitToInt (c: char) = int c - int '0'

let (++) (x, y) (x', y') = (x + x', y + y')

// aoc20:9
let pairs len =
    [| for i in 0..(len - 2) do for j in (i + 1)..(len - 1) do yield i, j |]

// aoc18:2
let rec pairCombos = function
    | [] | [_] -> []
    | head::tail -> List.map (fun e -> (head, e)) tail @ pairCombos tail

// aoc20:1
let cartpower lst n =
    let rec crtpwr n prd =
        if n = 0 then prd else
        crtpwr (n - 1) (prd |> Seq.collect (fun tpl ->
            lst |> Seq.map (fun e -> e::tpl)))
    crtpwr n (Seq.singleton [])

// aoc15:13
let rec permutations list =
    let rec insAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insAlong head) (permutations tail)

// aoc16:22
let adjcrds (x, y) width height =
    // [(0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1)]
    [(0, -1); (1, 0); (0, 1); (-1, 0)]
    |> List.map ((++) (x, y))
    |> List.filter (fun (x, y) -> 0 <= x && x < width && 0 <= y && y < height

// aoc18:20
let mapToString (map:Map<(int * int), int64>) =
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
