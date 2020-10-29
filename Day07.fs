open System
open System.Text.RegularExpressions

let text = IO.File.ReadAllText("Day07.txt")
let lines = Regex.Split(text, "\n(?=.)")

let wires = 
    let keywire (line: string) =
        let idx = line.IndexOf("->")
        let key = line.Substring(idx + 2).Trim()
        let expr = line.Substring(0, idx ).Trim()
        let wire = (Option<int>.None, expr)
        (key, wire)
    lines |> Seq.map keywire |> Map.ofSeq

let wvalue, wexpr = fst, snd
let evalue, ewires = fst, snd
let setvalue name n wire wires =
    (n, Map.add name (Some n, (wexpr wire)) wires)

let rec eval (expr: string) wires =
    match expr.Split(' ') with
    | [| _ |] ->
        match (Int32.TryParse expr) with
        | true, n -> (n, wires)
        | false, _ ->
            let name = expr
            let wire = Map.find name wires
            match wvalue wire with
            | Some n ->  (n, wires)
            | None ->            
                let n, wires = eval (wexpr wire) wires
                setvalue name n wire wires
    | [| "NOT"; pred |] ->
        let pred, wires = eval pred wires
        (~~~ pred, wires)        
    | [| left; op; right |] ->
        let left, wires = eval left wires
        let right, wires = eval right wires
        match op with
        | "AND" -> (left &&& right, wires)
        | "OR" -> (left ||| right, wires)
        | "LSHIFT" -> (left <<< right, wires)
        | "RSHIFT" -> (left >>> right, wires)
        | _ -> failwithf "Unknown operator: %s" op
    | _ -> failwithf "Don't know how to evaluate: \"%s\"" expr

[<EntryPoint>]
let main argv =
    let a, _ = eval "a" wires
    a |> printfn "Part 1 %A"
  
    let _, wires = setvalue "b" a wires.["b"] wires
    let a, _ = eval "a" wires
    a |> printfn "Part 2 %A"
    0
