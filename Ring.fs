module Ring

// aoc2017:17
module Ring =
    type Link<'a> = { mutable Item: 'a; mutable Next: Link<'a> }

    let singleton item =
        let rec link = { Item = item; Next = link }
        link

    let insert item link =
        link.Next <- { Item = item; Next = link.Next }
        link.Next

    let rec advance n link = if n = 0 then link else advance (n - 1) link.Next
    let rec find item link = 
        if item = link.Item then link else find item link.Next
    let item link = link.Item
    let next link = link.Next
