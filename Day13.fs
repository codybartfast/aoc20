let lines = System.IO.File.ReadAllLines("Day13.txt")
let arvlTime = lines.[0] |> int64
let busses =
    lines.[1].Split(',')
    |> Array.mapi (fun i s -> if s = "x" then None else Some (int64 i, int64 s))
    |> Array.choose id |> Array.toList

let wait bussId = (bussId - (arvlTime % bussId)) % bussId

let part1 () =
    busses
    |> Seq.map (fun (_, busId) -> (busId, wait busId))
    |> Seq.minBy (snd)
    |> fun (busId, wait) -> busId * wait

let rec matchingDepature (depTime, period) (busStop, busId) =
    Seq.initInfinite (fun n -> (int64 n) * period + depTime)
    |> Seq.find (fun depTime -> (depTime + busStop) % busId = 0L)
    |> fun depTime -> depTime, period * busId

let part2 () = ((0L, 1L), busses) ||> List.fold matchingDepature |> fst

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
