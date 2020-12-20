open System
open System.Text.RegularExpressions
open Grid

type Transform = Rotate | Flip

let toBlocks (str: string) =
    Regex.Split(str.Trim(), @"(?:\r?\n){2,}") |> Array.toList
    |> List.map (fun str -> Regex.Split(str.Trim(), @"\r?\n") |> Array.toList)
let blocks = System.IO.File.ReadAllText("Day20.txt") |> toBlocks

let blockToTile block =
    let head::rest = block
    let id = Regex.Match(head, "\d+").Value |> int64
    let grid = rest |> List.toArray |> Grid.ofLines
    id, grid
let tiles = blocks |> List.map blockToTile |> Map

let toSignatures chars =
    let signature =
        Array.map (function '.' -> '0' | _ -> '1')
        >> (fun binChars -> Convert.ToInt32(String binChars, 2))
    signature chars, (chars |> Array.rev |> signature)

let tileToCross (id, (grid: Grid<char>)) =
    let edges =
        [grid.Row 0; grid.Col 9; grid.Row 9; grid.Col 0]
        |> List.map toSignatures
    id, edges, []
let crosses = tiles |> Map.toList |> List.map tileToCross

let ident (ident, _, _) = ident
let top (_, [t; r; b; l], _) = t |> fst
let right (_, [t; r; b; l], _) = r  |> fst
let bottom (_, [t; r; b; l], _) = b  |> fst
let left (_, [t; r; b; l], _) = l  |> fst
let history (_, _, history) = history

let faces = [(top, bottom); (right, left); (bottom, top); (left, right)]

let rotate (id, [(a, ra); (b, rb); (c, rc); (d, rd)], hst) =
    (id, [(rd, d); (a, ra); (rb, b); (c, rc)], Rotate::hst)
let flip (id, [(a, ra); (b, rb); (c, rc); (d, rd)], hst) =
    (id, [(ra, a); (d, rd); (rc, c); (b, rb)], Flip::hst)

let remove cross = List.filter (fun other -> ident other <> ident cross)

let variations cross =
    [rotate; rotate; rotate; (rotate >> flip); rotate; rotate; rotate]
    |> List.scan (fun cross transform -> transform cross) cross

let canMatch cross =
    variations
    >> List.collect(fun t2 ->
        cross |> variations |> List.map (fun t1 -> (t1, t2)))
    >> List.exists(fun (t1, t2) ->
        faces |> List.exists (fun (f1, f2) -> f1 t1 = f2 t2))

let matchCount crosses cross =
    crosses |> remove cross |> List.filter (canMatch cross) |> List.length

let corners = crosses |> List.filter (matchCount crosses >> ((= ) 2))

let part1 () = corners |> List.map ident |> List.reduce (*)

let findNext edge face =
    List.collect variations >> List.filter (face >> (=) edge)

let topLeft =
    let rec orientate cross others =
        let noRight = findNext (right cross) left others |> List.isEmpty
        let noBottom = findNext (bottom cross) top others |> List.isEmpty
        if noRight || noBottom then orientate (rotate cross) others else cross
    let tl = corners.Head
    (remove tl crosses) |> (orientate tl)

let rec extend crosses tface others oface =
    match findNext (tface (List.head crosses)) oface others with
    | [] -> List.rev crosses
    | [next] -> extend (next::crosses) tface (remove next others) oface
    | _ -> failwith "oops"

let matchedGrid =
    extend [topLeft] bottom (remove topLeft crosses) top
    |> List.map(fun leftMost ->
        extend [leftMost] right (remove leftMost crosses) left)

let crossToTile cross =
    (tiles.[ident cross], history cross |> List.rev)
    ||> List.fold (fun tile trans ->
        match trans with
        | Rotate -> tile.Rotate()
        | Flip -> tile.FlipHorizontal ())

let joinRow (tiles: Grid<char> list) =
    [0 .. tiles.Head.Height - 1]
    |> List.map
        (fun y -> tiles |> List.map (Grid.row y) |> Array.concat)

let image =
    let crop tile = tile |> Grid.crop (1, 1) (tile.Width - 2, tile.Height - 2)
    matchedGrid
    |> List.collect (List.map (crossToTile >> crop) >> joinRow)
    |> List.toArray |> Grid

let nessy =
    [|  "                  # ";
        "#    ##    ##    ###";
        " #  #  #  #  #  #   "; |]
let nessyWidth, nessyHeight = nessy.[0].Length, nessy.Length
let nessyCoords =
    nessy |> Grid.ofLines |> Grid.filter ((=) '#') |> Seq.map fst |> Seq.toList

let nessyAt (img: Grid<char>) coord =
    let (++) (x, y) (x', y') = (x + x', y + y')
    nessyCoords
    |> List.forall (((++) coord) >> img.Get >> ((=) '#'))

let countNessy (img: Grid<char>) =
    seq{ for x in [0 .. img.Width - nessyWidth] do
            for y in [0 .. img.Height - nessyHeight] do x, y }
    |> Seq.filter (nessyAt img) |> Seq.length

let rotateGrid (grid: Grid<char>) = grid.Rotate ()
let flipGrid (grid: Grid<char>) = grid.FlipHorizontal ()
let imgVariations img =
    let rotate, flip = rotateGrid, flipGrid
    [rotate; rotate; rotate; (rotate >> flip); rotate; rotate; rotate]
    |> List.scan (fun tile transform -> transform tile) img

let part2 () =
    let nessyImage =
        image
        |> imgVariations
        |> List.find (countNessy >> ((<) 0))
    let nessyCount = countNessy nessyImage
    nessyImage.Count ((=) '#') - (nessyCount * nessyCoords.Length)

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
