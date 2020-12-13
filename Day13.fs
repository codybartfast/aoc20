open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

let lines = System.IO.File.ReadAllLines("Day13.txt")
let atime = lines.[0] |> int
let btimes = 
    Regex.Matches(lines.[1], @"\d+")
    |> Seq.map(fun m -> m.Value |> int) 

let busses = 
    lines.[1].Split(',')
    |> Array.mapi (fun i s -> 
        match Int64.TryParse(s) with true, n -> Some (int64 i, n) | _ -> None)
    |> Array.choose id
    |> Array.toList

let wait atime btime = 
    (btime - (atime % btime)) % btime

let part1 () =
    btimes
    |> Seq.map (fun b -> (b, wait atime b))
    |> Seq.minBy (snd)
    |> fun (b, d) -> b * d

let rec wait2 deptTime period diff bussId =
    if (deptTime + diff) % bussId = 0L then 
        deptTime
    else
        wait2 (deptTime + period) period diff bussId

let rec theTime depTime busPairs period : Int64 =
    if List.isEmpty busPairs then depTime else
    let (prevPos, _), (pos, bussId) = busPairs.Head
    let diff = pos //- prevPos
    let depTime = wait2 depTime period diff bussId
    theTime depTime busPairs.Tail (period * bussId)

let part2 () = 
    let busPairs = (0L, 0L)::busses |> List.pairwise
    theTime 0L busPairs 1L

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
