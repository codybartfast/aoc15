open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day15.txt")
let lines = Regex.Split(text, "\n(?=.)")

let data = lines |> List.ofArray |> List.map  (fun line ->
    let fs = Regex.Split(line, @"[^-\w]+")
    int fs.[10], [fs.[2]; fs.[4]; fs.[6]; fs.[8]] |> List.map int)
let icals = fst
let iprops = snd

let rec variations ningredients spoons quantities =
    match ningredients with
    | 1 -> Seq.singleton (spoons::quantities)
    | _ ->
        seq{0 .. spoons}
        |> Seq.collect (fun spns ->
            (variations (ningredients - 1) (spoons - spns) (spns::quantities)))

let recipe ingredients quantities =
    (ingredients, quantities) ||> List.map2 (fun (cals, props) spns ->
        (cals * spns, props |> List.map ((*) spns)))

let score =
    List.map iprops
    >> List.transpose
    >> List.map (List.sum >> ((max) 0))
    >> List.reduce (*)

let calorieseq n = List.map icals >> List.sum >> ((=) n)

[<EntryPoint>]
let main argv =
    variations (data.Length) 100 []
    |> Seq.map ((recipe data) >> score)
    |> Seq.max
    |> printfn "Part 1 %A"

    variations (data.Length) 100 []
    |> Seq.map (recipe data)
    |> Seq.filter (calorieseq 500)
    |> Seq.map score
    |> Seq.max
    |> printfn "Part 2 %A"
    0
