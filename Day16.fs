open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day16.txt")
let lines = Regex.Split(text, "\n(?=.)")

let aunts = lines |> Array.map (fun line ->
    let fs = Regex.Split(line, @"\W+")
    fs.[1], [(fs.[2], int fs.[3]); (fs.[4], int fs.[5]); (fs.[6], int fs.[7])] )
let number = fst
let known = snd

let mfcsam =
    [
        ("children", 3);
        ("cats", 7);
        ("samoyeds", 2);
        ("pomeranians", 3);
        ("akitas", 0);
        ("vizslas", 0);
        ("goldfish", 5);
        ("trees", 3);
        ("cars", 2);
        ("perfumes", 1)
    ]

let exactmatch = List.forall (Set mfcsam).Contains

let fuzzymatch =
    let map = Map mfcsam
    List.forall (fun (prop, n) ->
        let compare =
            match prop with
            | "cats" | "trees" -> (>)
            | "pomeranians" | "goldfish" -> (<)
            | _ -> (=)
        compare n map.[prop])

let find matcher = Seq.filter (known >> matcher) >> Seq.exactlyOne >> number

[<EntryPoint>]
let main argv =
    aunts
    |> find exactmatch
    |> printfn "Part 1 %A"

    aunts
    |> find fuzzymatch
    |> printfn "Part 2 %A"
    0
