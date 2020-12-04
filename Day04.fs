open System.Text.RegularExpressions

let txt = System.IO.File.ReadAllText("Day04.txt")
let passports =
    Regex.Split(txt.Trim(), @"(?:\r?\n){2,}")
    |> Array.toList
    |> List.map (fun record ->
        Regex.Split(record, @"\s+")
        |> Array.map (fun kvp -> kvp.Split(':') |> (fun a -> (a.[0], a.[1])))
        |> Map)

let triage pport =
    match Map.count pport with
    | 8 -> true
    | 7 when not (pport.ContainsKey "cid") -> true
    | _ -> false

let between low high = int >> (fun n -> low <= n && n <= high)
let chkRange low high key pport = between low high (Map.find key pport)
let chkExpr regex key pport = Regex.IsMatch(Map.find key pport, $"^{regex}$")

let byr = chkRange 1920 2002 "byr"
let iyr = chkRange 2010 2020 "iyr"
let eyr = chkRange 2020 2030 "eyr"
let hgt pport =
    let m = Regex.Match(Map.find "hgt" pport, @"(\d+)(cm|in)")
    match m.Success, m.Groups.[1].Value, m.Groups.[2].Value with
    | true, height, "cm" when between 150 193 height-> true
    | true, height, "in" when between 59 76 height -> true
    | _ -> false
let hcl = chkExpr @"#[\da-f]{6,6}" "hcl"
let ecl = chkExpr @"amb|blu|brn|gry|grn|hzl|oth" "ecl"
let pid = chkExpr @"\d{9,9}" "pid"

let valid pport =
    [triage; byr; iyr; eyr; hgt; hcl; ecl; pid]
    |> List.forall (fun check -> check pport)

let part1 = passports |> List.filter triage |> List.length
let part2 = passports |> List.filter valid |> List.length

[<EntryPoint>]
let main _ =
    part1 |> printfn "Part 1: %d"
    part2 |> printfn "Part 2: %d"
    0
