module Ring

// aoc2017:17
module Ring =
    type Link<'a> = { mutable Item: 'a; mutable Prev: Link<'a>; mutable Next: Link<'a> }

    let singleton item =
        let rec link = { Item = item; Prev = link; Next = link }
        link

    let insert item link =
        let next = link.Next
        link.Next <- { Item = item; Prev = link; Next = next }
        next.Prev <- link.Next
        link.Next



    let rec advance n link = if n = 0 then link else advance (n - 1) link.Next
    let rec find item link =
        if item = link.Item then link else find item link.Next
    let item link = link.Item
    let next link = link.Next

    let toList link =
        let last = link.Item
        let rec toList link lst =
            if link.Item = last then (link.Item::lst) else
            toList link.Prev (link.Item::lst)
        toList link.Prev []
