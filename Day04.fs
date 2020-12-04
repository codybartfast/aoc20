open System
open System.Text.RegularExpressions

type Passport = Map<string, string>

let inline toChars (str: string) = str.ToCharArray()


let raw = System.IO.File.ReadAllText("Day04.txt")
let lines = Regex.Split(raw, @"(\s*\n\s*){2,}") |> Array.toList
let parse (ln: string) = ln //.Split(' ')
let records = lines |> List.map parse |> List.filter (fun str -> not (Regex.IsMatch(str, @"^\s+$")))
let passports = 
    records
    |> List.map (fun rc -> 
        Regex.Split(rc, @"[;\s+]+")
        |> Array.map (fun str -> 
            str.Split(':')
            |> (fun [| key; value|] -> (key, value)))
        |> Map)


let plausible pspt = 
    match Map.count pspt with
    | 8 -> true
    | 7 when pspt.ContainsKey "cid" |> not -> true
    | _ -> false

let part1 =
    passports  |> List.filter plausible |> List.length


let byr (p: Passport) =    
    match Map.tryFind "byr" p with
    | None -> false
    | Some str ->
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 1920 <= n && n <= 2002
    
let iyr (p: Passport) =
    match Map.tryFind "iyr" p with
    | None -> false
    | Some str ->
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 2010 <= n && n <= 2020
    
let eyr (p: Passport) = 
    match Map.tryFind "eyr" p with
    | None -> false
    | Some str ->
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 2020 <= n && n <= 2030
    
let hgt (p: Passport) = 
    match Map.tryFind "hgt" p with
    | None -> false
    | Some str ->
    let m = Regex.Match(str, @"^(\d\d\d?)(cm|in)$")
    if not m.Success then false else
    match m.Groups.[2].Value, m.Groups.[1].Value |> int with
    | "cm", ht when 150 <= ht && ht <= 193 -> true
    | "in", ht when 59 <= ht && ht <= 76 -> true
    | _ -> false
    
let hcl (p: Passport) = 
    match Map.tryFind "hcl" p with
    | None -> false
    | Some str ->
    Regex.IsMatch(str, @"^\#[\da-f]{6,6}$")
    
let ecl (p: Passport) = 
    match Map.tryFind "ecl" p with
    | None -> false
    | Some str ->
    Regex.IsMatch(str, @"^(amb|blu|brn|gry|grn|hzl|oth)$")

let pid (p: Passport) = 
    match Map.tryFind "pid" p with
    | None -> false
    | Some str ->
    Regex.IsMatch(str, @"^\d{9,9}$")

let part2 =
    passports  
    |> List.filter 
        (fun p -> plausible p && byr p && iyr p && eyr p && hgt p &&
            hcl p && ecl p && pid p)
    |> List.length

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %A"
    0
