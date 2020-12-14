open System
open System.Text.RegularExpressions

type Bitmasks = {Or: uint64; And: uint64; Float: uint64}
type Line = Write of (uint64 * uint64) | Mask of Bitmasks

let lines = System.IO.File.ReadAllLines("Day14.txt")
let parse (ln: string) =
    if ln.StartsWith "mask" then
        let mask = ln.[7..]
        Mask { Or = Convert.ToUInt64(mask.Replace('X', '0'), 2)
               And = Convert.ToUInt64(mask.Replace('X', '1'), 2)
               Float = Convert.ToUInt64(
                        mask.Replace('1', '0').Replace('X', '1'), 2) }
    else
        let m = Regex.Match(ln, @"(\d+)\D+(\d+)")
        Write (m.Groups.[1].Value |> uint64, m.Groups.[2].Value |> uint64)        
let (Mask firstMask)::otherInput = lines |> Array.map parse |> Array.toList
let sumValues = Map.toSeq >> Seq.map snd >> Seq.sum

let rec decode mask input (memory: Map<uint64, UInt64>)=
    match input with
    | [] -> memory
    | (Mask masks)::rest -> decode masks rest memory
    | (Write (addr, value))::rest ->
        decode mask rest (memory.Add (addr, value ||| mask.Or &&& mask.And))

let part1 () = decode firstMask otherInput Map.empty|> sumValues

let expandMask addrs mask =
    let expandBit addrs bit =
        if mask &&& bit = 0UL then addrs else
        addrs |> List.collect (fun addr -> [ addr ||| bit; addr &&& ~~~bit])
    1UL 
    |> Seq.unfold (fun bit -> if bit > mask then None else Some(bit, bit * 2UL))
    |> Seq.fold expandBit addrs

let rec decodeV2 masks input (memory: Map<uint64, UInt64>) =
    match input with
    | [] -> memory
    | (Mask masks)::rest -> decodeV2 masks rest memory
    | (Write (addr, value))::rest ->
        (memory, expandMask [addr ||| masks.Or] masks.Float)
        ||> List.fold(fun memory addr -> memory.Add (addr, value))
        |> decodeV2 masks rest

let part2 () = decodeV2 firstMask otherInput Map.empty |> sumValues

[<EntryPoint>]
let main _ = printfn "Part 1: %d" (part1 ()); printfn "Part 2: %d" (part2 ()); 0
