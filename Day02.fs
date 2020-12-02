open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines("Day02.txt") |> Array.toList
let parse ln =
    let [| mn; mx; c; pwd |] = Regex.Split(ln, @"[: -]+")
    (c.[0], (int mn, int mx)), pwd
let passwords = lines |> List.map parse

let validDownTheStreet ((c, (mn, mx)), pwd) =
    let n = Regex.Matches(pwd, c.ToString()).Count
    mn <= n && n <= mx

let validHere ((c, (mn, mx)), (pwd: string)) =
    (pwd.[mn - 1] = c) = (pwd.[mx - 1] <> c)

let part1 = passwords |> List.filter validDownTheStreet |> List.length
let part2 = passwords |> List.filter validHere |> List.length

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
