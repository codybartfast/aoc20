open System
open System.Text.RegularExpressions

type Rule = Leaf of string | Comp of (int list) | Alt of  (int list * int list)

let toBlocks (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n){2,}")
let toLines (str: string) = Regex.Split(str.Trim(), @"(?:\r?\n)") |> Array.toList
let inline toChars (str: string) = str.ToCharArray()
let blocks = System.IO.File.ReadAllText("Day19.txt") |> toBlocks

let toNums (str: string ) = str.Split(' ') |> Array.map int |> Array.toList
let parseRule ln = 
    let [| idx; rule |] = Regex.Split(ln, ": ")
    let idx = int idx
    match Regex.Split (rule, @" \| ") with
    | [| "\"a\"" |] -> idx, Leaf ("a")
    | [| "\"b\"" |] -> idx, Leaf ("b")
    | [| lst |] -> idx, Comp (toNums lst)
    | [| lst1; lst2 |] -> idx, Alt ((toNums lst1, toNums lst2))

let rules = blocks.[0] |> toLines |> List.map parseRule |> Map

let messages = blocks.[1] |> toLines
let maxLen = messages |> Seq.map (fun m -> m.Length) |> Seq.max

let substrings ln =
    let chars =  toChars ln
    (Set.empty, [1..ln.Length]) ||> List.fold (fun set len ->
        chars |> Array.windowed len |> Array.map String |> Set |> Set.union set)
let allSubstrings = messages |> List.map substrings |> List.reduce (Set.union)
    
let trim msgs = msgs |> Seq.filter (fun (m: string) ->
    m.Length <= maxLen && allSubstrings.Contains m)

let cartProd (msgs1: Set<string>) (msgs2: Set<string>) =
    msgs1 |> Seq.collect(fun msg1 -> msgs2 |> Seq.map (fun msg2 -> msg1 + msg2))
    |> trim
    |> Set

let goodMessages rules =
    let rec expand known idx =
        match Map.tryFind idx known with
        | Some msgs -> known, msgs
        | _ ->
        match Map.find idx rules with
        | Leaf str ->
            let msgs = Set.singleton str
            Map.add idx msgs known, msgs
        | Comp idxLst ->
            let known, msgs = expandList known idxLst
            Map.add idx msgs known, msgs
        | Alt (idxLst1, idxLst2) ->
            if List.contains idx idxLst2 then expandLoop known idx idxLst1 idxLst2 else
            let known, msgs1 = expandList known idxLst1
            let known, msgs2 = expandList known idxLst2
            let msgs = Set.union msgs1 msgs2
            Map.add idx msgs known, msgs
    and expandList known idxLst =
        let known, msgsLst = ((known, []), idxLst) ||> List.fold (fun (known, msgsLst) rule ->
            let known, msgs = expand known rule
            known, msgs::msgsLst)
        known, (msgsLst |> List.rev |> List.reduce cartProd)

    and expandLoop known idx idxLst1 idxLst2 =
        let known, lhs = expandList known idxLst1
        let known, rhs = expandLoopRhs known idx idxLst2 lhs lhs
        let msgs = Set.union lhs rhs
        let known = Map.add idx msgs known
        known, msgs
    and expandLoopRhs known idx idxLst2 newMsgs allMsgs =
        if newMsgs.IsEmpty then known, allMsgs else
        let known = Map.add idx newMsgs known
        let known, newMsgs = expandList known idxLst2
        let newMsgs = Set.difference newMsgs allMsgs
        expandLoopRhs known idx idxLst2 newMsgs (Set.union allMsgs newMsgs)
    expand Map.empty 0 |> snd |> Set.intersect (Set messages) |> Set.count

let part1 () = rules |> goodMessages

let part2 () = 
    let add ln (rules: Map<int, Rule>) = rules.Add (parseRule ln)
    rules |> add "8: 42 | 42 8" |> add "11: 42 31 | 42 11 31" |> goodMessages

[<EntryPoint>]
// let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
let main _ =
    let sw = System.Diagnostics.Stopwatch ()
    sw.Start()
    printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ());
    sw.Stop()
    printfn "%f" sw.Elapsed.TotalSeconds
    0