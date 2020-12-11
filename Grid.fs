module Grid
open System

let nl = Environment.NewLine
let fchar = function '.' -> " " | c -> c |> string

type Grid<'a>(data: 'a[][]) =

    let width = data.[0].Length
    let height = data.Length

    let inbounds (x, y) = (x >= 0 && y >= 0 && x < width && y < height)

    member _.Data with get() = data
    member _.Width with get() = width
    member _.Height with get() = height

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

    member _.AdjacentUC((x, y)) =
        [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
        |> List.map(fun (x, y) -> (x, y), data.[y].[x])

    member _.Adjacent((x, y)) =
        // [(x + 1, y); (x, y + 1); (x - 1, y); (x, y - 1)]
        [(x, y + 1); (x + 1, y + 1); (x + 1, y); (x + 1, y - 1);
            (x, y - 1); (x - 1, y - 1); (x - 1, y); (x - 1, y + 1)] 
        |> List.filter inbounds
        |> List.map(fun (x, y) -> (x, y), data.[y].[x])

    // query

    member this.Filter(pred) = this.Flatten () |> Seq.filter (snd >> pred)
    member this.Find(pred) = this.Flatten () |> Seq.find (snd >> pred)
    member this.TryFind(pred) = this.Flatten () |> Seq.tryFind (snd >> pred)
    member _.Count(pred) = Seq.concat data|> Seq.filter pred |> Seq.length

///////////

    member this.Transform<'b  when 'b : equality>
        (generate: Grid<'a> -> int -> int -> 'b) : Grid<'b> =
            [| for y in 0 .. (height - 1) do
                [| for x in 0 .. (width - 1) do
                    generate this x y |] |]
            |> Grid<'b>


    // display

    member _.AsTextF<'a>(fmt) =
        String.concat nl (data |> Array.map (fun row ->
            String.concat "" (row |> Array.map fmt)))

    member this.AsText<'a>() =
        this.AsTextF(fun c -> c.ToString())

    override this.ToString() = this.AsText()

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
