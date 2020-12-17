let gens, mrgn = 6, 7
let lines = System.IO.File.ReadAllLines("Day17.txt")
let start =
    lines
    |> Array.mapi (fun y row -> row.ToCharArray () |> Array.mapi (fun x cube ->
        ((x + mrgn, y + mrgn, mrgn, mrgn), cube)))
    |> Array.collect id
    |> Array.filter (snd >> ((=) '#'))
    |> Array.map fst

let nhoodCoords =
    let cartpower lst n =
        let rec crtpwr n prd =
            if n = 0 then prd else
            crtpwr (n - 1) (prd |> Seq.collect (fun tpl ->
                lst |> Seq.map (fun e -> e::tpl)))
        crtpwr n (Seq.singleton [])
    cartpower [0; -1; 1] 4
    |> Seq.map (fun [x; y; z; w] -> (x, y, z, w)) |> Seq.toArray

let (++) (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')
let nhood coord =  nhoodCoords |> Array.map ((++) coord)
let adjacent  = nhood >> Array.tail
let cubesNear coords = coords |> Array.collect nhood |> Array.distinct

let next lookup cube =
    let count = adjacent cube |> Array.sumBy lookup
    match lookup cube, count with  1, 2 | 1, 3 | 0, 3 -> 1 | _ -> 0

let getLookup cubes =
    let len = lines.[0].Length + mrgn + mrgn
    let zet = Array4D.zeroCreate len len len len
    cubes |> Seq.iter (fun (x, y, z, w) -> Array4D.set zet x y z w 1)
    (fun (x, y, z, w) -> Array4D.get zet x y z w)

let evolve4D cubes =
    cubes |> cubesNear |> Array.filter (next (getLookup cubes) >> ((=) 1))
let evolve3D = evolve4D >> Array.filter (fun (_, _, _, w) -> w = mrgn)

let repeat fn n = List.init n (fun _ -> fn) |> List.reduce (>>)

let part1 () = start |> (repeat evolve3D gens) |> Seq.length
let part2 () = start |> (repeat evolve4D gens) |> Seq.length

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
