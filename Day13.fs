open System
open System.Text.RegularExpressions

let rec perms list =
    let rec insAlong i list =
        match list with
        | [] -> [[i]]
        | h::t -> (i::list)::(List.map (fun sub -> h::sub) (insAlong i t))
    match list with
    | [] -> [[]]
    | head::tail -> List.collect (insAlong head) (perms tail)

let text = IO.File.ReadAllText("Day13.txt")
let lines = Regex.Split(text, "\n(?=.)")

let data =
        lines |> Array.map (fun line ->
            let parts = Regex.Split(line, @"\W+")
            let sign = if (parts.[2] = "gain") then 1 else -1
            (parts.[0], parts.[10]), sign * (int parts.[3]) )

let datamap = Map data
let names = data |> Seq.map (fst >> fst) |> Seq.distinct |> List.ofSeq
let happiness (a, b) =  datamap.[a, b] + datamap.[b, a]
let happilist plan =  plan |> List.pairwise |> List.sumBy happiness
let happiloop plan = (happilist plan) + happiness (plan.Head, List.last plan)
let findhappiest names measure = names |> perms |> Seq.map measure |> Seq.max

[<EntryPoint>]
let main argv =
    findhappiest names happiloop
    |> printfn "Part 1 %A"

    findhappiest names happilist
    |> printfn "Part 2 %A"
    0
