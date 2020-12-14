open System
open System.Text.RegularExpressions

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let inline toChars (str: string) = str.ToCharArray()

type Line = Masks of UInt64 * UInt64 * UInt64 | Write of (uint64 * uint64)

let lines = System.IO.File.ReadAllLines("Day14.txt")
let parse (ln: string) =
    if ln.StartsWith "mask" then
        let mask = ln.[7..]
        let orMask = Convert.ToUInt64(mask.Replace('X', '0'), 2)
        let andMask = Convert.ToUInt64(mask.Replace('X', '1'), 2)
        let floatMask = Convert.ToUInt64(mask.Replace('1', '0').Replace('X', '1'), 2)
        Masks (orMask, andMask, floatMask)
    else
        let m = Regex.Match(ln, @"\[(\d+)] = (\d+)")
        Write (m.Groups.[1].Value |> uint64, m.Groups.[2].Value |> uint64)
let input = lines |> Array.map parse |> Array.toList
let memory =
    let maxAddr =
        input
        |> List.map (function Masks _-> 0UL | Write (addr, _) -> addr) |> List.max
    // printfn "%A" maxAddr
    [|0UL .. (maxAddr * 2UL) |] |> Array.map (fun _ -> 0UL)

let rec runPart1 (memory: UInt64[]) masks input =
    match input with
    | [] -> memory
    | ((Masks _) as masks)::rest -> runPart1 memory masks rest
    | (Write (addr, value))::rest ->
        let (Masks (orMask, andMask, _)) = masks
        memory.[int addr] <- (value ||| orMask) &&& andMask
        runPart1 memory masks rest

let part1 () = runPart1 (Array.copy memory) input.Head input.Tail |> Array.sum

let rec expandAddress addr addrs mask bit =
    if bit > mask then addrs else
    match mask &&& bit with
    | 0UL -> expandAddress addr addrs mask (bit * 2UL)
    | _ ->
        let comp = ~~~bit
        let addrs0 = addrs |> List.map (fun addr -> addr &&& comp)
        let addrs1 = addrs |> List.map (fun addr -> addr ||| bit)
        expandAddress addr (addrs0 @ addrs1) mask (bit * 2UL)

let rec run (memory: Map<uint64, UInt64>) masks input =
    match input with
    | [] -> memory
    | ((Masks _) as masks)::rest -> run memory masks rest
    | (Write (addr, value))::rest ->
        let (Masks (orMask, andMask, floatMask)) = masks
        let addr = (addr ||| orMask) //&&& andMask
        printfn "addr %d " addr
        let memory = 
            (memory, expandAddress addr [addr] floatMask 1UL)
            ||> List.fold(fun memory addr ->
                printfn "Writing %A to %A" value addr
                memory.Add (addr, value))
        run memory masks rest
let part2 () = run Map.empty input.Head input.Tail |> Map.toSeq |> Seq.map snd |> Seq.sum

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
