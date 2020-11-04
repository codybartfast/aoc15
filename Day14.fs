open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day14.txt")
let lines = Regex.Split(text, "\n(?=.)")

let distance speed fly rest time =
    let period = fly + rest;
    let nperiod = time / period
    let rmnd = time - (nperiod * period)
    speed * (nperiod * fly + min rmnd fly)

let data = lines |> Array.map (fun line ->
    let fs = Regex.Split(line, @"\W+")
    fs.[0], (distance (int fs.[3]) (int fs.[7]) (int fs.[14])))

let winners dears time =
    dears
    |> Seq.map (fun (name, dist) -> name, dist time)
    |> Seq.groupBy snd
    |> Seq.maxBy fst
    |> (snd >> Seq.map (fst))

[<EntryPoint>]
let main argv =
    data
    |> Seq.map (fun (_, dist) -> dist 2503)
    |> Seq.max
    |> printfn "Part 1 %A"

    seq{1 .. 2503}
    |> Seq.collect (winners data)
    |> Seq.groupBy id
    |> Seq.map (snd >> Seq.length)
    |> Seq.max
    |> printfn "Part 2 %A"
    0
