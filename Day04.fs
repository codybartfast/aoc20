open System
open System.Text.RegularExpressions

type Passport = Map<string, string>

let txt = System.IO.File.ReadAllText("Day04.txt")
let passports = 
    Regex.Split(txt, @"\r?\n\r?\n") |> Array.toList    
    |> List.map (fun record -> 
        Regex.Split(record, @"\s+")
        |> Array.map (fun kvp -> kvp.Split(':') |> (fun a -> (a.[0], a.[1])))
        |> Map)

let triage pspt = 
    match Map.count pspt with
    | 8 -> true
    | 7 when not (pspt.ContainsKey "cid") -> true
    | _ -> false

let part1 = passports |> List.filter triage |> List.length

let byr (p: Passport) =    
    let str = Map.find "byr" p
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 1920 <= n && n <= 2002
    
let iyr (p: Passport) =
    let str = Map.find "iyr" p
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 2010 <= n && n <= 2020
    
let eyr (p: Passport) = 
    let str = Map.find "eyr" p
    match Int32.TryParse str with
    | false, _ -> false
    | true, n -> 2020 <= n && n <= 2030
    
let hgt (p: Passport) = 
    let str = Map.find "hgt" p
    let m = Regex.Match(str, @"^(\d\d\d?)(cm|in)$")
    if not m.Success then false else
    match m.Groups.[2].Value, m.Groups.[1].Value |> int with
    | "cm", ht when 150 <= ht && ht <= 193 -> true
    | "in", ht when 59 <= ht && ht <= 76 -> true
    | _ -> false
    
let hcl (p: Passport) = 
    let str = Map.find "hcl" p
    Regex.IsMatch(str, @"^\#[\da-f]{6,6}$")
    
let ecl (p: Passport) = 
    let str = Map.find "ecl" p
    Regex.IsMatch(str, @"^(amb|blu|brn|gry|grn|hzl|oth)$")

let pid (p: Passport) = 
    let str = Map.find "pid" p
    Regex.IsMatch(str, @"^\d{9,9}$")

let part2 =
    passports  
    |> List.filter 
        (fun p -> triage p && byr p && iyr p && eyr p && hgt p &&
            hcl p && ecl p && pid p)
    |> List.length

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %A"
    part2 |> printfn "Part 2: %d"
    0
