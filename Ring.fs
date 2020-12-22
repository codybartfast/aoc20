module Ring

type RingLink<'a> =
    { mutable Item: 'a;
      mutable Prev: RingLink<'a>;
      mutable Next: RingLink<'a> }
type Ring<'a> = Ring of RingLink<'a> | EmptyRing

// aoc2017:17
module Ring =
    let refEq a b = LanguagePrimitives.PhysicalEquality a b
    let empty = EmptyRing

    let singleton item =
        let rec link = { Item = item; Prev = link; Next = link }
        Ring link

    let insert item = function
        | EmptyRing -> singleton item
        | Ring link ->
            let next = link.Next
            link.Next <- { Item = item; Prev = link; Next = next }
            next.Prev <- link.Next
            Ring link.Next

    let remove ring =
        match ring with
        | EmptyRing -> failwith "Tried to remove from an empty ring"
        | (Ring link) ->
            if refEq link.Next link then EmptyRing else
            link.Prev.Next <- link.Next
            link.Next.Prev <- link.Prev
            Ring link.Next

    let item = function
        | EmptyRing -> failwith "Tried to get item from empty ring"
        | Ring link -> link.Item
    let next = function
        | EmptyRing -> EmptyRing
        | Ring link -> Ring link.Next
    let prev = function
        | EmptyRing -> EmptyRing
        | Ring link -> Ring link.Prev
    let rec advance n = function
        | EmptyRing -> EmptyRing
        | ring -> if n = 0 then ring else advance (n - 1) (next ring)
    let rec reverse n = function
        | EmptyRing -> EmptyRing
        | ring -> if n = 0 then ring else reverse (n - 1) (prev ring)
    let rec find value ring =
        if value = item ring then ring else find value (next ring)

    let ofList =
        let rec ofList rng = function
            [] -> advance 1 rng | h::t -> ofList (insert h rng) t
        function [] -> EmptyRing | lst -> ofList EmptyRing lst

    let toList = function
        | EmptyRing -> []
        | Ring start ->
            let rec toList link lst =
                if refEq start link then (link.Item::lst) else
                toList link.Prev (link.Item::lst)
            toList start.Prev []
