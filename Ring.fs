module Ring

type Link<'a> =
    { mutable Item: 'a;
      mutable Prev: Link<'a>;
      mutable Next: Link<'a> }
// aoc2017:17
module Ring =
    let refEq a b = LanguagePrimitives.PhysicalEquality a b

    let singleton item =
        let rec link = { Item = item; Prev = link; Next = link }
        link

    let insertLink ln link =
        ln.Prev <- link
        ln.Next <- link.Next
        link.Next <- ln
        ln.Next.Prev <- ln
        ln

    let insert item link =
        let next = link.Next
        link.Next <- { Item = item; Prev = link; Next = next }
        next.Prev <- link.Next
        link.Next

    let remove link =
        if refEq link.Next link then failwith "Can't remove last item of ring" else
        link.Prev.Next <- link.Next
        link.Next.Prev <- link.Prev
        link.Next

    let rec advance n link = if n = 0 then link else advance (n - 1) link.Next
    let rec reverse n link = if n = 0 then link else reverse (n - 1) link.Prev
    let rec find item link =
        if item = link.Item then link else find item link.Next
    let item link = link.Item
    let next link = link.Next

    let ofList =
        let rec ofList rng = function
            [] -> rng.Next | h::t -> ofList (insert h rng) t
        function
        | [] -> failwith "I Don't do empty rings"
        | h::t -> ofList (singleton h) t

    let toLinks start =
        let rec toList link lst =
            if refEq start link then (link::lst) else
            toList link.Prev (link::lst)
        toList start.Prev []

    let toList start = start |> toLinks |>  List.map item
