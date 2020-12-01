let expns =
    System.IO.File.ReadAllLines("Day01.txt")
    |> Array.map int |> Array.toList

let tuples lst n =
    let rec tuples n tpls =
        if n = 0 then tpls else
            tuples (n - 1) (tpls |> Seq.collect (fun tpl ->
                        lst |> Seq.map (fun e -> e::tpl)))
    tuples n (Seq.singleton [])

let entries = tuples expns >> Seq.filter (List.sum >> ((=) 2020)) >> Seq.head

let part1 = entries 2 |> List.reduce (*)
let part2 = entries 3 |> List.reduce (*)

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
