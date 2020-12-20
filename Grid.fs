module Grid
open System

let nl = Environment.NewLine
let fchar = function '.' -> " " | c -> c |> string

type Grid<'a when 'a : equality>(data: 'a[][]) =

    let width = data.[0].Length
    let height = data.Length

    // nearby
    let (++) (x, y) (x', y') = (x + x', y + y')
    let inRange (x, y) = (x >= 0 && y >= 0 && x < width && y < height)
    let vBoadering = [(0, -1); (1, 0); (0, 1); (-1, 0)]
    let vAdjacent =
        [(0, -1); (1, -1); (1, 0); (1, 1); (0, 1); (-1, 1); (-1, 0); (-1, -1)]
    let mGetUC (x, y) =
        List.map ((++) (x, y) >> (fun (x, y) -> (x, y), data.[y].[x]))
    let mGet (x, y) =
        List.map ((++) (x, y))
        >> List.filter inRange
        >> List.map(fun (x, y) -> (x, y), data.[y].[x])

    member _.Data with get() = data
    member _.Width with get() = width
    member _.Height with get() = height

    // construction

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> (int * int) -> 'a -> 'b) =
            data
            |> Array.mapi(fun y row ->
                row |> Array.mapi(fun x v -> generate this (x, y) v))
            |> Grid

    member _.Crop((x, y), (width, height)) =
        data.[y .. (y + height - 1)]
        |> Array.map (fun row -> row.[x .. (x + width - 1)])
        |> Grid<'a>

    member _.Rotate() = data |> Array.transpose |> Array.map (Array.rev) |> Grid

    member _.FlipHorizontal() = data |> Array.map (Array.rev) |> Grid

    // accessors

    member _.Item
        with get(x, y) = data.[y].[x]
        and set(x, y) v = data.[y].[x] <- v

    member this.Get((x, y)) = this.[x, y]
    member this.Set((x, y), v) = this.[x, y] <- v
    member _.Row(y) = data.[y] |> Array.copy
    member _.Col(x) = data |> Array.map (fun arr -> arr.[x])

    member _.Flatten() =
        data |> Seq.mapi (fun y row -> (y, row)) |> Seq.collect (fun (y, row) ->
            row |> Seq.mapi (fun x v -> ((x, y), v)))

    // nearby
    member _.Adjacent (x, y) = mGet (x, y) vAdjacent
    member _.AdjacentUC (x, y) = mGetUC (x, y) vAdjacent

    member _.Bordering (x, y) = mGet (x, y) vBoadering
    member _.BorderingUC (x, y) = mGetUC (x, y) vBoadering

    // query

    member this.Filter(pred) = this.Flatten () |> Seq.filter (snd >> pred)
    member this.Find(pred) = this.Flatten () |> Seq.find (snd >> pred)
    member this.TryFind(pred) = this.Flatten () |> Seq.tryFind (snd >> pred)
    member _.Count(pred) = Seq.concat data|> Seq.filter pred |> Seq.length
    member _.InRange (coord) = inRange coord

    // display

    member _.AsTextF<'a>(fmt) =
        String.concat nl (data |> Array.map (fun row ->
            String.concat "" (row |> Array.map fmt)))

    member this.AsText<'a>() =
        this.AsTextF(fun c -> c.ToString())

    override this.ToString() = this.AsText()

    // comparison
    override this.GetHashCode() = 0

    override this.Equals (other) =
        match other with
        | :? Grid<'a> as grid -> this.Data = grid.Data
        | _ -> false

// =============================================================================

module Grid =

    // construction

    let ofLines (lines: string[]) =
        let rows = lines |> Array.map (fun s -> s.ToCharArray())
        Grid(rows)

    let init width height (gen: int -> int -> 'a) =
        [| for y in 0 .. (height - 1) do
            [| for x in 0 .. (width - 1) do
                gen x y |] |]
        |> Grid<'a>

    let initWith width height def = init width  height (fun _ _ -> def)

    // accessors

    let flatten (grid: Grid<'a>) = grid.Flatten ()

    // query

    let filter pred (grid: Grid<'a>) = grid.Filter pred
    let find pred (grid: Grid<'a>) = grid.Find pred
    let tryFind pred (grid: Grid<'a>) = grid.TryFind pred
    let count pred (grid: Grid<'a>) = grid.Count pred

    // display

    let astext (grid: Grid<'a>) = grid.AsText()
    let astextf fmt (grid: Grid<'a>) = grid.AsTextF(fmt)
