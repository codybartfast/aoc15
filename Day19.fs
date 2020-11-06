open System

let text = IO.File.ReadAllText("Day19.txt")
let lines = text.Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
let medicine = (Array.last lines).ToCharArray() |> Array.toList
let replmap =
    (lines.[.. (lines.Length - 2)])
    |> Seq.map (fun s ->
        let [|f; t|] = s.Split(" => ")
        (f.ToCharArray() |> Array.toList,
            t.ToCharArray() |> Array.toList |> List.rev))
    |> Seq.groupBy fst
    |> Seq.map (fun (f, pairs) -> (f, (pairs |> Seq.map snd |> Seq.toList)))
    |> Map

let lookup chars =
    match replmap.TryFind chars with None _ -> [] | Some rpls -> rpls

let rec combine left right =
    match left with [] -> right | a::left -> combine left (a::right)

let altheads right =
    let replace n =
        let right' = List.skip n right
        lookup (List.take n right)
        |>  List.map ((fun sub -> combine sub right'))
    match right with
    | [] -> Seq.empty
    | [_] -> replace 1 |> List.toSeq
    | _ -> Seq.concat [replace 1; replace 2]

let splits list =
    let rec iter left right = seq {
        yield left, right
        match right with [] -> () | a::right -> yield! iter (a::left) right }
    iter [] list

let allchildren =
    splits >> Seq.collect (fun (l, r) -> r |> altheads |> Seq.map (combine l))

let nextsources ((tlen, target), source) = seq{
    match target, source with
    | [], _ | _, [] -> ()
    | t::ts, s::ss ->
        if t = s then
            yield (tlen - 1, ts), ss
        yield! source |> altheads |> Seq.map (fun src -> (tlen, target), src) }

let sort = 
    Seq.distinct >> Seq.groupBy (fst>>fst) >> Seq.sortBy fst >> Seq.collect snd

let fabricate molecule source maxtrials =
    let rec fab steps sources =
        let next = sources |> Seq.collect nextsources 
        let sorted = next |> sort |> Seq.truncate maxtrials |> Seq.toList
        match sorted with
        | ((0,_),_)::_ -> steps
        | _ -> fab (steps + 1) sorted
    let len = List.length molecule
    fab 1 [((len, molecule), source)]
    |> (fun steps -> steps - len)

[<EntryPoint>]
let main argv =
    medicine
    |> allchildren
    |> Seq.distinct
    |> Seq.length
    |> printfn "Part 1 %A"

    fabricate medicine ['e'] 100
    |> printfn "Part 2 %A"
    0
