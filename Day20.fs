open System
open System.Text.RegularExpressions
open Grid

type Trans = R | F

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}") |> Array.toList
let toLines (str: string) = Regex.Split(str.Trim(), @"\r?\n") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let toNum (line: char[]) =
    line
    |> Array.map (function '.' -> '0' | _ -> '1')
    |> String
    |> (fun s -> Convert.ToInt32(s, 2))

let toPair (line: char[]) = toNum line, (line |> Array.rev |> toNum)

let blocks = System.IO.File.ReadAllText("Day20.txt") |> toBlocks

let parse block =
    let head::rest = toLines block
    let id = Regex.Match(head, "\d+").Value |> int64
    let grid = rest |> List.toArray |> Grid.ofLines
    // id, grid
    id, grid.Crop ((1, 1), (grid.Width - 2, grid.Height - 2))

let tiles = blocks |> List.map parse |> Map

let parseSimp block =
    let head::rest = toLines block
    let id = Regex.Match(head, "\d+").Value |> int64
    let grid = rest |> List.toArray |> Grid.ofLines
    let edges = [grid.Row 0; grid.Col 9; grid.Row 9; grid.Col 0] |> List.map toPair
    id, edges, []

let simpTiles = blocks |> List.map parseSimp

let ident (ident, _, _) = ident
let top (_, [t; r; b; l], _) = t |> fst
let right (_, [t; r; b; l], _) = r  |> fst
let bottom (_, [t; r; b; l], _) = b  |> fst
let left (_, [t; r; b; l], _) = l  |> fst
let history (_, _, history) = history

let faces = [(top, bottom); (right, left); (bottom, top); (left, right)]

let rotate (id, [(a, ra); (b, rb); (c, rc); (d, rd)], hst) =
    (id, [(rd, d); (a, ra); (rb, b); (c, rc)], R::hst)
let flip (id, [(a, ra); (b, rb); (c, rc); (d, rd)], hst) =
    (id, [(ra, a); (d, rd); (rc, c); (b, rb)], F::hst)

let variations tile =
    [rotate; rotate; rotate; (rotate >> flip); rotate; rotate; rotate]
    |> List.scan (fun tile transform -> transform tile) tile

// let variations tile = trackedVariations tile |> List.map fst

let canMatch tile1 tile2 =
    let t2Vars = tile2 |> variations
    tile1
    |> variations
    |> List.collect(fun t1 -> t2Vars |> List.map (fun t2 -> (t1, t2)))
    |> List.exists(fun (t1, t2) -> faces |> List.exists (fun (f1, f2) -> f1 t1 = f2 t2))

let matchCount tiles tile =
    tiles
    |> List.filter ((<>) tile)
    |> List.filter (canMatch tile)
    |> List.length

let corners = simpTiles |> List.filter ((matchCount simpTiles) >> ((= ) 2))

let part1 () = corners |> List.map ident |> List.reduce (*)

let getMatch target face tiles =
    tiles
    |> List.tryPick (fun tile -> tile |> variations |> List.tryFind (face ((=) target)))

let remove tiles tile  = tiles |> List.filter (fun other -> ident other <> ident tile)

let findNext edge face others =
    others
    |> List.collect (fun other -> variations other |> List.map (fun var -> other, var))
    |> List.filter (fun (other, var) -> edge = face var)
    |> List.map snd

let topLeft =
    let rec orientate tile others =
        let noRight = findNext (right tile) left others |> List.isEmpty
        let noBottom = findNext (bottom tile) top others |> List.isEmpty
        if noRight || noBottom then orientate (rotate tile) others else tile
    let tl = corners.Head
    tl |> (remove simpTiles) |> (orientate tl)

let rec extend tiles tface others oface =
    match findNext (tface (List.head tiles)) oface others with
    | [] -> List.rev tiles
    | [next] -> extend (next::tiles) tface (remove others next) oface
    | _ -> failwith "oops"

let matchedGrid =
    let leftEdge = extend [topLeft] bottom (remove simpTiles topLeft) top
    let rows =
        leftEdge
        |> List.map(fun leftMost ->
            extend [leftMost] right (remove simpTiles leftMost) left)
    rows

let simpToTile simp =
    (tiles.[ident simp], history simp |> List.rev)
    ||> List.fold (fun tile trans ->
        match trans with R -> tile.Rotate() | F -> tile.FlipHorizontal ())

let joinRow (tiles: Grid<char> list) =
    [0 .. tiles.Head.Height - 1]
    |> List.map (fun y -> tiles |> List.map (fun tile -> tile.Row y) |> Array.concat)

let image =
    matchedGrid
    |> List.collect (fun row -> row |> List.map simpToTile |> joinRow)
    |> List.toArray |> Grid

let nessy =
    [|  "                  # ";
        "#    ##    ##    ###";
        " #  #  #  #  #  #   "; |]
let nessyWidth, nessyHeight = nessy.[0].Length, nessy.Length

let nessyCoords =
    nessy
    |> Grid.ofLines
    |> Grid.filter ((=) '#')
    |> Seq.map fst
    |> Seq.toList

let nessyAt (img: Grid<char>) coord =
    let (++) (x, y) (x', y') = (x + x', y + y')
    nessyCoords
    |> List.forall (((++) coord) >> img.Get >> ((=) '#'))

let countNessy (img: Grid<char>) =
    seq{ for x in [0 .. img.Width - nessyWidth] do
            for y in [0 .. img.Height - nessyHeight] do x, y }
    |> Seq.filter (nessyAt img)
    |> Seq.length

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
        |> List.filter (countNessy >> ((<) 0))
        |> List.exactlyOne
    let count = countNessy nessyImage
    nessyImage.Count ((=) '#') - (count * ((nessy |> Grid.ofLines).Count ((=) '#')))


[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0