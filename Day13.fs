let lines = System.IO.File.ReadAllLines("Day13.txt")
let arvlTime = lines.[0] |> int64
let busses =
    lines.[1].Split(',')
    |> Array.mapi (fun i s -> if s = "x" then None else Some (int64 i, int64 s))
    |> Array.choose id
    |> Array.toList

let wait bussId = (bussId - (arvlTime % bussId)) % bussId

let part1 () =
    busses
    |> Seq.map (fun (_, busId) -> (busId, wait busId))
    |> Seq.minBy (snd)
    |> fun (busId, d) -> busId * d

let rec matchWait deptTime period (busStop, bussId) =
    match (deptTime + busStop) % bussId with
    | 0L -> deptTime, period * bussId
    | _ -> matchWait (deptTime + period) period (busStop, bussId)

let rec winTime (depTime, period) busPairs  =
    match busPairs with
    | [] -> depTime
    | bus::rest -> winTime (matchWait depTime period bus) rest

let part2 () = winTime (0L, 1L) busses

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
