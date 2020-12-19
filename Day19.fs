open System
open System.Text.RegularExpressions

type Rule = 
    | Leaf of string
    | Comp of (int list)
    | Alt of  (int list * int list)

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n)") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()

let blocks = System.IO.File.ReadAllText("Day19.txt") |> toBlocks
let toNums (str: string ) = str.Split(' ') |> Array.map int |> Array.toList
let rules = 
    blocks.[0]
    |> toLines
    |> List.map (fun str -> 
        let [| idx; rule |] = Regex.Split(str, ": ")
        let idx = int idx
        match Regex.Split (rule, @" \| ") with
        | [| "\"a\"" |] -> idx, Leaf ("a")
        | [| "\"b\"" |] -> idx, Leaf ("b")
        | [| lst |] -> idx, Comp (toNums lst)
        | [| lst1; lst2 |] -> idx, Alt ((toNums lst1, toNums lst2)))
    |> Map

let messgase = blocks.[1] |> toLines |> Set
    
   
let cartProd (msgs1: Set<string>) (msgs2: Set<string>) =
    msgs1 |> Seq.collect(fun msg1 -> msgs2 |> Seq.map (fun msg2 -> msg1 + msg2))
    |> Set

let rec expand known idx =
    match Map.tryFind idx known with
    | Some msgs -> known, msgs
    | _ ->
    match rules.[idx] with
    | Leaf str -> 
        let msgs = Set.singleton str
        Map.add idx msgs known, msgs
    | Comp idxLst -> 
        let known, msgs = expandList known idxLst
        Map.add idx msgs known, msgs
    | Alt (idxLst1, idxLst2) -> 
        let known, msgs1 = expandList known idxLst1
        let known, msgs2 = expandList known idxLst2
        let msgs = Set.union msgs1 msgs2
        Map.add idx msgs known, msgs
and expandList known idxLst =
    let known, msgsLst = ((known, []), idxLst) ||> List.fold (fun (known, msgsLst) rule ->
        let known, msgs = expand known rule
        known, msgs::msgsLst)
    known, (msgsLst |> List.rev |> List.reduce cartProd)


let part1 () =
    expand Map.empty 0 |> snd |> Set.intersect messgase |> Set.count

let part2 () = messgase

[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0