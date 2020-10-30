open System
open System.Text.RegularExpressions

let rec perms lst =
    let rec insAlong e lst =
        match lst with
        | [] -> [[e]]
        | h::t -> (e::lst) :: ((insAlong e t) |> List.map (fun l -> h::l))
    match lst with
    | [] -> [[]]
    | h::t -> (perms t) |> List.collect (insAlong h)

let text = IO.File.ReadAllText("Day09.txt")
let lines =
    Regex.Split(text, "\r?\n(?=.)")

type Leg = ((string * string) * int)
let leg (line: string) : Leg =
    match line.Split(' ') with
    | [| a; _ ; b ; _ ; d|] -> ((a, b), int d)
    | _ -> failwith "oops"
let start : Leg -> string = fst >> fst
let finish  : Leg -> string= fst >> snd
let len  : Leg -> int = snd

let legs =  lines |> Array.map leg |> List.ofArray
let locs =
    legs
    |> List.collect (fun loc -> [start loc; finish loc])
    |> List.distinct
let lens =
    legs
    |> Seq.collect (fun leg -> [((start leg, finish leg), len leg); ((finish leg, start leg), len leg)] )
    |> Map.ofSeq
let routeLen route =
    let rec iter route len =
        match route with
        | [] | [_] -> len
        | a::b::t -> iter (b::t) (len + lens.[(a, b)])
    iter route 0
let routeLens =
    locs
    |> perms
    |> List.map routeLen

[<EntryPoint>]
let main argv =
    routeLens
    |> Seq.min
    |> printfn "Part 1 %A"

    routeLens
    |> Seq.max
    |> printfn "Part 2 %A"
    0
