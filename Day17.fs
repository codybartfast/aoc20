let gens, mrgn = 6, 7
let lines = System.IO.File.ReadAllLines("Day17.txt")
let initialActive =
    lines
    |> Array.mapi (fun y row -> row.ToCharArray () |> Array.mapi (fun x chr ->
        ((x + mrgn, y + mrgn, mrgn, mrgn), chr)))
    |> Array.collect id
    |> Array.filter (snd >> ((=) '#'))
    |> Array.map fst

let getIsActive cubes =
    let len = lines.[0].Length + mrgn + mrgn
    let sparse = Array4D.zeroCreate len len len len
    cubes |> Seq.iter (fun (x, y, z, w) -> Array4D.set sparse x y z w true)
    (fun (x, y, z, w) -> Array4D.get sparse x y z w)

let nhoodCoords =
    let rec cartpower lst n  prd =
        if n = 0 then prd else
        cartpower lst (n - 1) (prd |> Seq.collect (fun tpl ->
            lst |> Seq.map (fun e -> e::tpl)))
    cartpower [0; -1; 1] 4 (Seq.singleton [])
    |> Seq.map (fun [x; y; z; w] -> (x, y, z, w)) |> Seq.toArray
let (++) (x, y, z, w) (x', y', z', w') = (x + x', y + y', z + z', w + w')
let nhood coord =  nhoodCoords |> Array.map ((++) coord)
let adjacent  = nhood >> Array.tail
let cubesNear coords = coords |> Array.collect nhood |> Array.distinct

let evolveCube isActive cube =
    let count = adjacent cube |> Array.filter isActive |> Array.length
    match isActive cube, count with
    |true, 2 | true, 3 | false, 3 -> true
    | _ -> false

let evolve4D cubes =
    cubes |> cubesNear |> Array.filter (evolveCube (getIsActive cubes))
let evolve3D = evolve4D >> Array.filter (fun (_, _, _, w) -> w = mrgn)
let repeat fn n = List.init n (fun _ -> fn) |> List.reduce (>>)
let finalSize evolve = initialActive |> repeat evolve gens |> Seq.length

let part1 () = finalSize evolve3D
let part2 () = finalSize evolve4D

[<EntryPoint>]
let main _ = printfn "Part 1: %A" (part1 ()); printfn "Part 2: %A" (part2 ()); 0
