open System

let text = IO.File.ReadAllText("Day24.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let packages = lines |> Seq.map int64 |> Seq.rev |> Seq.toList
let load = Seq.sum packages

let entanglement = Seq.reduce (*)

let rec pack target unpacked packed =
    let mutable minsollen = packages.Length
    let comp2min sollen = if sollen < minsollen then minsollen <- sollen
    let rec iter capacity unpacked packed =
        let plen = List.length packed
        if capacity < 0L || plen > minsollen then Seq.empty
        elif capacity = 0L then comp2min plen; Seq.singleton packed
        else
            match unpacked with
            | [] -> Seq.empty
            | h::t -> Seq.concat [
                iter (capacity - h) t (h::packed);
                iter capacity t packed ]
    iter target unpacked packed

let passols target packages =
    let passsols = pack target packages []
    let minsolen = passsols |> Seq.map Seq.length |> Seq.min
    passsols
    |> Seq.filter (Seq.length >> ((=) minsolen))
    |> Seq.sortBy entanglement
    |> Seq.toList

let remaining sol = packages |> List.filter ((Set.ofList sol).Contains >> not)

let firstWithRemainingSol target solUsedLst =
    let hasSol = passols target >> Seq.isEmpty >> not
    solUsedLst
    |> Seq.map (fun (sol, used) -> sol, remaining used)
    |> Seq.filter (snd >> hasSol)
    |> Seq.head |> fst

[<EntryPoint>]
let main argv =
    let target = load / 3L
    passols target packages
    |> Seq.map (fun sol -> sol, sol)
    |> firstWithRemainingSol target
    |> entanglement
    |> printfn "Part 1 %A"

    let target = load / 4L
    passols target packages
    |> Seq.collect (fun sol ->
        remaining sol
        |> (passols target)
        |> List.map (fun sidesol -> sol, List.append sol sidesol))
    |> firstWithRemainingSol target
    |> entanglement
    |> printfn "Part 2 %A"
    0
