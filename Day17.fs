open System

let text = IO.File.ReadAllText("Day17.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let containers = lines |> Array.toList |> List.map int |> List.sortDescending

let rec fill eggnog used unused =
    match eggnog, unused with
    | 0, _ -> Seq.singleton used
    | _, [] -> Seq.empty
    | en, _ when en < 0 -> Seq.empty
    | en, cn::unsd ->
        Seq.concat [ fill (en - cn) (cn::used) unsd;
                     fill en used unsd ]

[<EntryPoint>]
let main argv =
    fill 150 [] containers
    |> Seq.length
    |> printfn "Part 1 %A"

    fill 150 [] containers
    |> Seq.groupBy Seq.length
    |> Seq.minBy fst
    |> snd |> Seq.length
    |> printfn "Part 2 %A"
    0
